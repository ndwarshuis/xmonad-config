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

import           Data.Either
import           Data.List
import           Data.Maybe

import           DBus.Client
import           DBus.Internal

import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process
    ( readProcessWithExitCode
    )

import           Xmobar.Plugins.Bluetooth
import           Xmobar.Plugins.ClevoKeyboard
import           Xmobar.Plugins.Device
import           Xmobar.Plugins.IntelBacklight
import           Xmobar.Plugins.Screensaver
import           Xmobar.Plugins.VPN

import           XMonad.Core
    ( cfgDir
    , getDirectories
    )
import           XMonad.Hooks.DynamicLog                        (wrap)
import           XMonad.Internal.Command.Power                  (hasBattery)
import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Screensaver               (ssSignalDep)
import           XMonad.Internal.Dependency
import           XMonad.Internal.Shell
import qualified XMonad.Internal.Theme                          as T
import           Xmobar
import           Xmobar.Plugins.Common

main :: IO ()
main = do
  sysClient <- getDBusClient True
  sesClient <- getDBusClient False
  ff <- evalFonts
  cs <- getAllCommands =<< rightPlugins sysClient sesClient
  d <- cfgDir <$> getDirectories
  -- this is needed to see any printed messages
  hFlush stdout
  mapM_ (maybe skip disconnect) [sysClient, sesClient]
  xmobar $ config ff cs d

config :: (BarFont -> BarMetaFont) -> BarRegions -> String -> Config
config ff br confDir = defaultConfig
  { font = fontString ff firstFont
  , additionalFonts = fontString ff <$> restFonts
  , textOffset = fontOffset ff firstFont
  , textOffsets = fontOffset ff <$> restFonts
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

ethernetCmd :: String -> CmdSpec
ethernetCmd iface = CmdSpec
  { csAlias = iface
  , csRunnable = Run
    $ Device (iface, fontifyText IconMedium "\xf0e8", colors)
  }

batteryCmd :: CmdSpec
batteryCmd =  CmdSpec
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
    , "-o" , fontify "\xf0e7"
    , "-O" , fontify "\xf1e6"
    , "-i" , fontify "\xf1e6"
    ] 50
  }
  where
    fontify = fontifyText IconSmall

vpnCmd :: CmdSpec
vpnCmd = CmdSpec
  { csAlias = vpnAlias
  , csRunnable = Run $ VPN (fontifyText IconMedium "\xf023", colors)
  }

btCmd :: CmdSpec
btCmd = CmdSpec
  { csAlias = btAlias
  , csRunnable = Run
    $ Bluetooth (fontify "\xf5b0", fontify "\xf5ae") colors
  }
  where
    fontify = fontifyText IconLarge

alsaCmd :: CmdSpec
alsaCmd = CmdSpec
  { csAlias = "alsa:default:Master"
  , csRunnable = Run
    $ Alsa "default" "Master"
    [ "-t", "<status><volume>%"
    , "--"
    , "-O", fontifyText IconSmall "\xf028"
    , "-o", fontifyText IconSmall "\xf026 "
    , "-c", T.fgColor
    , "-C", T.fgColor
    ]
  }

blCmd :: CmdSpec
blCmd = CmdSpec
  { csAlias = blAlias
  , csRunnable = Run $ IntelBacklight $ fontifyText IconSmall "\xf185"
  }

ckCmd :: CmdSpec
ckCmd = CmdSpec
  { csAlias = ckAlias
  , csRunnable = Run $ ClevoKeyboard $ fontifyText IconSmall "\xf40b"
  }

ssCmd :: CmdSpec
ssCmd = CmdSpec
  { csAlias = ssAlias
  , csRunnable = Run
    $ Screensaver (fontifyText IconSmall "\xf254", colors)
  }

lockCmd :: CmdSpec
lockCmd = CmdSpec
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
    numIcon = fontify "\xf8a5"
    capIcon = fontify "\xf657"
    fontify = fontifyText IconXLarge
    disabledColor = xmobarFGColor T.backdropFgColor

dateCmd :: CmdSpec
dateCmd = CmdSpec
  { csAlias = "date"
  , csRunnable = Run $ Date "%Y-%m-%d %H:%M:%S " "date" 10
  }

--------------------------------------------------------------------------------
-- | command runtime checks and setup
--
-- some commands depend on the presence of interfaces that can only be
-- determined at runtime; define these checks here
--
-- in the case of network interfaces, assume that the system uses systemd in
-- which case ethernet interfaces always start with "en" and wireless
-- interfaces always start with "wl"

type BarFeature = Sometimes CmdSpec

isWireless :: String -> Bool
isWireless ('w':'l':_) = True
isWireless _           = False

isEthernet :: String -> Bool
isEthernet ('e':'n':_) = True
isEthernet _           = False

listInterfaces :: IO [String]
listInterfaces = fromRight [] <$> tryIOError (listDirectory sysfsNet)

sysfsNet :: FilePath
sysfsNet = "/sys/class/net"

readInterface :: String -> (String -> Bool) -> IODependency String
readInterface n f = IORead n go
  where
    go = do
      ns <- filter f <$> listInterfaces
      case ns of
        [] -> return $ Left ["no interfaces found"]
        (x:xs) -> do
          return $ Right $ PostPass x $ fmap ("ignoring extra interface: "++) xs

vpnPresent :: IO (Maybe String)
vpnPresent = go <$> tryIOError (readProcessWithExitCode "nmcli" args "")
  where
    args = ["-c", "no", "-t", "-f", "TYPE", "c", "show"]
    go (Right (ExitSuccess, out, _))   = if "vpn" `elem` lines out then Nothing
                                         else Just "vpn not found"
    go (Right (ExitFailure c, _, err)) = Just $ "vpn search exited with code "
                                         ++ show c ++ ": " ++ err
    go (Left e)                        = Just $ show e

xmobarDBus :: String -> DBusDependency_ -> CmdSpec -> Maybe Client -> BarFeature
xmobarDBus n dep cmd cl = sometimesDBus cl n "xmobar dbus interface"
  (Only_ dep) $ const cmd

rightPlugins :: Maybe Client -> Maybe Client -> IO [Maybe CmdSpec]
rightPlugins sysClient sesClient = mapM evalFeature
  [ Left getWireless
  , Left $ getEthernet sysClient
  , Left $ getVPN sysClient
  , Left $ getBt sysClient
  , Left getAlsa
  , Left getBattery
  , Left $ getBl sesClient
  , Left $ getCk sesClient
  , Left $ getSs sesClient
  , always' "lock indicator" lockCmd
  , always' "date indicator" dateCmd
  ]
  where
    always' n = Right . Always n . Always_ . FallbackAlone

getWireless :: BarFeature
getWireless = sometimes1 "wireless status indicator" "sysfs path"
  $ IORoot wirelessCmd
  $ Only $ readInterface "get wifi interface" isWireless

getEthernet :: Maybe Client -> BarFeature
getEthernet client = sometimes1 "ethernet status indicator" "sysfs path"
  $ DBusRoot (const . ethernetCmd) tree client
  where
    tree = And1 (Only readEth) (Only_ devDep)
    readEth = readInterface "read ethernet interface" isEthernet

getBattery :: BarFeature
getBattery = sometimesIO_ "battery level indicator" "sysfs path"
  (Only_ $ sysTest "Test if battery is present" hasBattery)
  batteryCmd

getVPN :: Maybe Client -> BarFeature
getVPN client = sometimesDBus client "VPN status indicator"
  "xmobar dbus interface" (toAnd vpnDep test) (const vpnCmd)
  where
    test = DBusIO $ sysTest "Use nmcli to test if VPN is present" vpnPresent

getBt :: Maybe Client -> BarFeature
getBt = xmobarDBus "bluetooth status indicator" btDep btCmd

getAlsa :: BarFeature
getAlsa = sometimesIO_ "volume level indicator" "alsactl"
  (Only_ $ sysExe "alsactl") alsaCmd

getBl :: Maybe Client -> BarFeature
getBl = xmobarDBus "Intel backlight indicator" intelBacklightSignalDep blCmd

getCk :: Maybe Client -> BarFeature
getCk = xmobarDBus "Clevo keyboard indicator" clevoKeyboardSignalDep ckCmd

getSs :: Maybe Client -> BarFeature
getSs = xmobarDBus "screensaver indicator" ssSignalDep ssCmd

getAllCommands :: [Maybe CmdSpec] -> IO BarRegions
getAllCommands right = do
  let left =
        [ CmdSpec
          { csAlias = "UnsafeStdinReader"
          , csRunnable = Run UnsafeStdinReader
          }
        ]
  return $ BarRegions
    { brLeft = left
    , brCenter = []
    , brRight = catMaybes right
    }

--------------------------------------------------------------------------------
-- | fonts

data BarFont = Text
  | IconSmall
  | IconMedium
  | IconLarge
  | IconXLarge
  deriving (Eq, Enum, Bounded, Show)

data BarMetaFont = BarMetaFont
  { bfOffset   :: Int
  , bfBuilder  :: T.FontBuilder
  , bfFontData :: T.FontData
  }

-- font data ~ (offset, fontification string)
fontString :: (BarFont -> BarMetaFont) -> BarFont -> String
fontString f bf = b d
  where
    b = bfBuilder $ f bf
    d = bfFontData $ f bf

fontOffset :: (BarFont -> BarMetaFont) -> BarFont -> Int
fontOffset f = bfOffset . f

firstFont :: BarFont
firstFont = minBound

restFonts :: [BarFont]
restFonts = enumFrom $ succ minBound

-- allFonts :: [BarFont]
-- allFonts = enumFrom minBound

-- allFontOffsets :: [Int]
-- allFontOffsets = fontOffset <$> allFonts

-- allFontStrings :: [String]
-- allFontStrings = fontString <$> allFonts

barFont :: Always T.FontBuilder
barFont = T.fontFeature "XMobar Text Font" "DejaVu Sans Mono"

nerdFont :: Always T.FontBuilder
nerdFont = T.fontFeature "XMobar Icon Font" "Symbols Nerd Font"

evalFonts :: IO (BarFont -> BarMetaFont)
evalFonts = do
  bf <- evalAlways barFont
  nf <- evalAlways nerdFont
  return $ fontData bf nf

fontData :: T.FontBuilder -> T.FontBuilder -> BarFont -> BarMetaFont
fontData barBuilder nerdBuilder bf = case bf of
  Text       -> BarMetaFont 16 barBuilder
                $ T.defFontData { T.weight = Just T.Bold, T.size = Just 11 }
  IconSmall  -> nerd 16 13
  IconMedium -> nerd 17 15
  IconLarge  -> nerd 17 18
  IconXLarge -> nerd 18 20
  where
    nerd o s = BarMetaFont o nerdBuilder
      $ T.defFontData { T.pixelsize = Just s, T.size = Nothing }

-- barFont :: Always T.FontBuilder
-- barFont = T.fmtFontXFT T.font
--   { T.family = "DejaVu Sans Mono"
--   , T.size = Just 11
--   , T.weight = Just T.Bold
--   }

-- nerdFont :: Int -> String
-- nerdFont size = T.fmtFontXFT T.font
--   { T.family = "Symbols Nerd Font"
--   , T.size = Nothing
--   , T.pixelsize = Just size
--   }

fontifyText :: BarFont -> String -> String
fontifyText fnt txt = concat ["<fn=", show $ fromEnum fnt, ">", txt, "</fn>"]

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
