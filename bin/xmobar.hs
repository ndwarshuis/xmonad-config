module Main (main) where

--------------------------------------------------------------------------------
-- | Xmobar binary
--
-- Features:
-- * Uses the 'UnsafeStdinReader' to receive the current workspace/layout config
--   from xmonad
-- * FontAwesome and Symbol fonts for icons
-- * Some custom plugins (imported below)
-- * Theme integration with xmonad (shared module imported below)
-- * A custom Locks plugin from my own forked repo

import           Data.List
import           Data.Maybe

import           DBus.Client

import           System.Exit
import           System.IO
import           System.IO.Error

import           Xmobar.Plugins.Bluetooth
import           Xmobar.Plugins.ClevoKeyboard
import           Xmobar.Plugins.Device
import           Xmobar.Plugins.IntelBacklight
import           Xmobar.Plugins.Screensaver
import           Xmobar.Plugins.VPN

import           System.Posix.Signals
import           XMonad.Core
    ( cfgDir
    , getDirectories
    , io
    )
import           XMonad.Hooks.DynamicLog                        (wrap)
import           XMonad.Internal.Command.Power                  (hasBattery)
import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Control
import           XMonad.Internal.DBus.Screensaver               (ssSignalDep)
import           XMonad.Internal.Dependency
import           XMonad.Internal.Process
    ( proc'
    , readCreateProcessWithExitCode'
    )
import qualified XMonad.Internal.Theme                          as T
import           Xmobar                                         hiding
    ( iconOffset
    )
import           Xmobar.Plugins.Common

main :: IO ()
main = do
  db <- connectDBus
  c <- withCache $ evalConfig db
  disconnectDBus db
  -- this is needed to prevent waitForProcess error when forking in plugins (eg
  -- alsacmd)
  _ <- installHandler sigCHLD Default Nothing
  -- this is needed to see any printed messages
  hFlush stdout
  xmobar c

evalConfig :: DBusState -> FIO Config
evalConfig db = do
  cs <- getAllCommands <$> rightPlugins db
  bf <- getTextFont
  (ifs, ios) <- getIconFonts
  d <- io $ cfgDir <$> getDirectories
  return $ config bf ifs ios cs d

--------------------------------------------------------------------------------
-- | toplevel configuration

-- | The text font family
textFont :: Always T.FontBuilder
textFont = fontAlways "XMobar Text Font" "DejaVu Sans Mono"

-- | Offset of the text in the bar
textFontOffset :: Int
textFontOffset = 16

-- | Attributes for the bar font (size, weight, etc)
textFontData :: T.FontData
textFontData = T.defFontData { T.weight = Just T.Bold, T.size = Just 11 }

-- | The icon font family
iconFont :: Sometimes T.FontBuilder
iconFont = fontSometimes "XMobar Icon Font" "Symbols Nerd Font"

-- | Offsets for the icons in the bar (relative to the text offset)
iconOffset :: BarFont -> Int
iconOffset IconSmall  = 0
iconOffset IconMedium = 1
iconOffset IconLarge  = 1
iconOffset IconXLarge = 2

-- | Sizes (in pixels) for the icon fonts
iconSize :: BarFont -> Int
iconSize IconSmall  = 13
iconSize IconMedium = 15
iconSize IconLarge  = 18
iconSize IconXLarge = 20

-- | Attributes for icon fonts
iconFontData :: Int -> T.FontData
iconFontData s = T.defFontData { T.pixelsize = Just s, T.size = Nothing }

-- | Global configuration
-- Note that the 'font' and 'textOffset' are assumed to pertain to one (and
-- only one) text font, and all other fonts are icon fonts. If this assumption
-- changes the code will need to change significantly
config :: String -> [String] -> [Int] -> BarRegions -> FilePath -> Config
config bf ifs ios br confDir = defaultConfig
  { font = bf
  , additionalFonts = ifs
  , textOffset = textFontOffset
  , textOffsets = ios
  , bgColor = T.bgColor
  , fgColor = T.fgColor
  , position = BottomSize C 100 24
  , border = NoBorder
  , borderColor = T.bordersColor

  , sepChar = pSep
  , alignSep = [lSep, rSep]
  , template = fmtRegions br

  , lowerOnStart = False
  , hideOnStart = False
  , allDesktops = True
  , overrideRedirect = True
  , pickBroadest = False
  , persistent = True
  -- store the icons with the xmonad/xmobar stack project
  , iconRoot = confDir ++ "/icons"

  , commands = csRunnable <$> concatRegions br
  }

--------------------------------------------------------------------------------
-- | plugin features
--
-- some commands depend on the presence of interfaces that can only be
-- determined at runtime; define these checks here

getAllCommands :: [Maybe CmdSpec] -> BarRegions
getAllCommands right = BarRegions
  { brLeft = [ CmdSpec
               { csAlias = "UnsafeStdinReader"
               , csRunnable = Run UnsafeStdinReader
               }
             ]
  , brCenter = []
  , brRight = catMaybes right
  }

rightPlugins :: DBusState -> FIO [Maybe CmdSpec]
rightPlugins DBusState { dbSesClient = ses, dbSysClient = sys }
  = mapM evalFeature
  [ Left getWireless
  , Left $ getEthernet sys
  , Left $ getVPN sys
  , Left $ getBt sys
  , Left getAlsa
  , Left getBattery
  , Left $ getBl ses
  , Left $ getCk ses
  , Left $ getSs ses
  , Right getLock
  , always' "date indicator" dateCmd
  ]
  where
    always' n = Right . Always n . Always_ . FallbackAlone

type BarFeature = Sometimes CmdSpec

-- TODO what if I don't have a wireless card?
getWireless :: BarFeature
getWireless = Sometimes "wireless status indicator" xpfWireless
  [Subfeature (IORoot wirelessCmd $ Only readWireless) "sysfs path"]

getEthernet :: Maybe Client -> BarFeature
getEthernet cl = iconDBus "ethernet status indicator" (const True) root tree
  where
    root useIcon tree' = DBusRoot (const . ethernetCmd useIcon) tree' cl
    tree = And1 (Only readEthernet) (Only_ devDep)

getBattery :: BarFeature
getBattery = iconIO_ "battery level indicator" xpfBattery root tree
  where
    root useIcon = IORoot_ (batteryCmd useIcon)
    tree = Only_ $ IOTest_ "Test if battery is present" $ fmap (Msg Error) <$> hasBattery

getVPN :: Maybe Client -> BarFeature
getVPN cl = iconDBus_ "VPN status indicator" xpfVPN root $ toAnd_ vpnDep test
  where
    root useIcon tree = DBusRoot_ (const $ vpnCmd useIcon) tree cl
    test = DBusIO $ IOTest_ "Use nmcli to test if VPN is present" vpnPresent

getBt :: Maybe Client -> BarFeature
getBt = xmobarDBus "bluetooth status indicator" xpfBluetooth btDep btCmd

getAlsa :: BarFeature
getAlsa = iconIO_ "volume level indicator" (const True) root
  $ Only_ $ sysExe "alsactl"
  where
    root useIcon = IORoot_ (alsaCmd useIcon)

getBl :: Maybe Client -> BarFeature
getBl = xmobarDBus "Intel backlight indicator" xpfIntelBacklight
  intelBacklightSignalDep blCmd

getCk :: Maybe Client -> BarFeature
getCk = xmobarDBus "Clevo keyboard indicator" xpfClevoBacklight
  clevoKeyboardSignalDep ckCmd

getSs :: Maybe Client -> BarFeature
getSs = xmobarDBus "screensaver indicator" (const True) ssSignalDep ssCmd

getLock :: Always CmdSpec
getLock = always1 "lock indicator" "icon indicator" root $ lockCmd fontifyAlt
  where
    root = IORoot_ (lockCmd fontifyIcon) $ Only_ iconDependency

--------------------------------------------------------------------------------
-- | bar feature constructors

xmobarDBus :: String -> XPQuery -> DBusDependency_ -> (Fontifier -> CmdSpec)
  -> Maybe Client -> BarFeature
xmobarDBus n q dep cmd cl = iconDBus_ n q root (Only_ dep)
  where
    root useIcon tree = DBusRoot_ (const $ cmd useIcon) tree cl

iconIO_ :: String -> XPQuery -> (Fontifier -> IOTree_ -> Root CmdSpec)
  -> IOTree_ -> BarFeature
iconIO_ = iconSometimes' And_ Only_

iconDBus :: String -> XPQuery -> (Fontifier -> DBusTree p -> Root CmdSpec)
  -> DBusTree p -> BarFeature
iconDBus = iconSometimes' And1 $ Only_ . DBusIO

iconDBus_ :: String -> XPQuery -> (Fontifier -> DBusTree_ -> Root CmdSpec)
  -> DBusTree_ -> BarFeature
iconDBus_ = iconSometimes' And_ $ Only_ . DBusIO

iconSometimes' :: (t -> t_ -> t) -> (IODependency_ -> t_) -> String -> XPQuery
  -> (Fontifier -> t -> Root CmdSpec) -> t -> BarFeature
iconSometimes' c d n q r t = Sometimes n q
  [ Subfeature icon "icon indicator"
  , Subfeature text "text indicator"
  ]
  where
    icon = r fontifyIcon $ c t $ d iconDependency
    text = r fontifyAlt t

--------------------------------------------------------------------------------
-- | command specifications

data BarRegions = BarRegions
  { brLeft   :: [CmdSpec]
  , brCenter :: [CmdSpec]
  , brRight  :: [CmdSpec]
  } deriving Show

data CmdSpec = CmdSpec
  { csAlias    :: String
  , csRunnable :: Runnable
  } deriving Show

concatRegions :: BarRegions -> [CmdSpec]
concatRegions (BarRegions l c r) = l ++ c ++ r

wirelessCmd :: String -> CmdSpec
wirelessCmd iface = CmdSpec
  { csAlias = iface ++ "wi"
  , csRunnable = Run
    $ Wireless iface
    [ "-t", "<qualityipat><essid>"
    , "--"
    , "--quality-icon-pattern", "<icon=wifi_%%.xpm/>"
    ] 5
  }

ethernetCmd :: Fontifier -> String -> CmdSpec
ethernetCmd fontify iface = CmdSpec
  { csAlias = iface
  , csRunnable = Run
    $ Device (iface, fontify IconMedium "\xf0e8" "ETH", colors)
  }

batteryCmd :: Fontifier -> CmdSpec
batteryCmd fontify = CmdSpec
  { csAlias = "battery"
  , csRunnable = Run
    $ Battery
    [ "--template", "<acstatus><left>"
    , "--Low", "10"
    , "--High", "80"
    , "--low", "red"
    , "--normal", T.fgColor
    , "--high", T.fgColor
    , "--"
    , "-P"
    , "-o" , fontify' "\xf0e7" "BAT"
    , "-O" , fontify' "\xf1e6" "AC"
    , "-i" , fontify' "\xf1e6" "AC"
    ] 50
  }
  where
    fontify' = fontify IconSmall

vpnCmd :: Fontifier -> CmdSpec
vpnCmd fontify = CmdSpec
  { csAlias = vpnAlias
  , csRunnable = Run $ VPN (fontify IconMedium "\xf023" "VPN", colors)
  }

btCmd :: Fontifier -> CmdSpec
btCmd fontify = CmdSpec
  { csAlias = btAlias
  , csRunnable = Run
    $ Bluetooth (fontify' "\xf5b0" "+", fontify' "\xf5ae" "-") colors
  }
  where
    fontify' i = fontify IconLarge i . ("BT" ++)

alsaCmd :: Fontifier -> CmdSpec
alsaCmd fontify = CmdSpec
  { csAlias = "alsa:default:Master"
  , csRunnable = Run
    $ Alsa "default" "Master"
    [ "-t", "<status><volume>%"
    , "--"
    , "-O", fontify' "\xf028" "+"
    , "-o", fontify' "\xf026" "-" ++ " "
    , "-c", T.fgColor
    , "-C", T.fgColor
    ]
  }
  where
    fontify' i = fontify IconSmall i . ("VOL" ++)

blCmd :: Fontifier -> CmdSpec
blCmd fontify = CmdSpec
  { csAlias = blAlias
  , csRunnable = Run $ IntelBacklight $ fontify IconSmall "\xf185" "BL: "
  }

ckCmd :: Fontifier -> CmdSpec
ckCmd fontify = CmdSpec
  { csAlias = ckAlias
  , csRunnable = Run $ ClevoKeyboard $ fontify IconSmall "\xf40b" "KB: "
  }

ssCmd :: Fontifier -> CmdSpec
ssCmd fontify = CmdSpec
  { csAlias = ssAlias
  , csRunnable = Run $ Screensaver (fontify IconSmall "\xf254" "SS", colors)
  }

lockCmd :: Fontifier -> CmdSpec
lockCmd fontify = CmdSpec
  { csAlias = "locks"
  , csRunnable = Run
    $ Locks
    [ "-N", numIcon
    , "-n", disabledColor numIcon
    , "-C", capIcon
    , "-c", disabledColor capIcon
    , "-s", ""
    , "-S", ""
    , "-d", " "
    ]
  }
  where
    numIcon = fontify' "\xf8a5" "N"
    capIcon = fontify' "\xf657" "C"
    fontify' = fontify IconXLarge
    disabledColor = xmobarFGColor T.backdropFgColor

dateCmd :: CmdSpec
dateCmd = CmdSpec
  { csAlias = "date"
  , csRunnable = Run $ Date "%Y-%m-%d %H:%M:%S " "date" 10
  }

--------------------------------------------------------------------------------
-- | low-level testing functions

vpnPresent :: IO (Maybe Msg)
vpnPresent =
  go <$> tryIOError (readCreateProcessWithExitCode' (proc' "nmcli" args) "")
  where
    args = ["-c", "no", "-t", "-f", "TYPE", "c", "show"]
    go (Right (ExitSuccess, out, _))   = if "vpn" `elem` lines out then Nothing
                                         else Just $ Msg Error "vpn not found"
    go (Right (ExitFailure c, _, err)) = Just $ Msg Error
                                         $ "vpn search exited with code "
                                         ++ show c ++ ": " ++ err
    go (Left e)                        = Just $ Msg Error $ show e

--------------------------------------------------------------------------------
-- | text font
--
-- ASSUME there is only one text font for this entire configuration. This
-- will correspond to the first font/offset parameters in the config record.

getTextFont :: FIO String
getTextFont = do
  fb <- evalAlways textFont
  return $ fb textFontData

--------------------------------------------------------------------------------
-- | icon fonts

getIconFonts :: FIO ([String], [Int])
getIconFonts = do
  fb <- evalSometimes iconFont
  return $ maybe ([], []) apply fb
  where
    apply fb = unzip $ (\i -> (iconString fb i, iconOffset i + textFontOffset))
      <$> iconFonts

data BarFont = IconSmall
  | IconMedium
  | IconLarge
  | IconXLarge
  deriving (Eq, Enum, Bounded, Show)

iconFonts :: [BarFont]
iconFonts = enumFrom minBound

iconString :: T.FontBuilder -> BarFont -> String
iconString fb i = fb $ iconFontData $ iconSize i

iconDependency :: IODependency_
iconDependency = IOSometimes_ iconFont

fontifyText :: BarFont -> String -> String
fontifyText fnt txt = concat ["<fn=", show $ 1 + fromEnum fnt, ">", txt, "</fn>"]

type Fontifier = BarFont -> String -> String -> String

fontifyAlt :: Fontifier
fontifyAlt _ _ alt = alt

fontifyIcon :: Fontifier
fontifyIcon f i _ = fontifyText f i

--------------------------------------------------------------------------------
-- | various formatting things

colors :: Colors
colors = Colors { colorsOn = T.fgColor, colorsOff = T.backdropFgColor }

sep :: String
sep = xmobarFGColor T.backdropFgColor " : "

lSep :: Char
lSep = '}'

rSep :: Char
rSep = '{'

pSep :: String
pSep = "%"

fmtSpecs :: [CmdSpec] -> String
fmtSpecs = intercalate sep . fmap go
  where
    go CmdSpec { csAlias = a } = wrap pSep pSep a

fmtRegions :: BarRegions -> String
fmtRegions BarRegions { brLeft = l, brCenter = c, brRight = r } =
  fmtSpecs l ++ [lSep] ++ fmtSpecs c ++ [rSep] ++ fmtSpecs r
