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

import           Control.Monad

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

import           XMonad                                         (getXMonadDir)
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
  cs <- getAllCommands =<< rightPlugins sysClient sesClient
  d <- getXMonadDir
  -- this is needed to see any printed messages
  hFlush stdout
  mapM_ (maybe skip disconnect) [sysClient, sesClient]
  xmobar $ config cs d

config :: BarRegions -> String -> Config
config br confDir = defaultConfig
  -- TODO head makes me feel icky
  { font = head allFontStrings
  , additionalFonts = drop 1 allFontStrings
  , textOffset = head allFontOffsets
  , textOffsets = drop 1 allFontOffsets
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
    , "-a", notify
    , "-P"
    , "-o" , fontify "\xf0e7"
    , "-O" , fontify "\xf1e6"
    , "-i" , fontify "\xf1e6"
    ] 50
  }
  where
    fontify = fontifyText IconSmall
    -- TODO format this with standardized notification library from xmonad.internal
    notify = fmtCmd "notify-send"
      [ "-u"
      , "critical"
      , "-i"
      , "'dialog-information-symbolic'"
      , "'low battery'"
      ]

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

type BarFeature = Feature CmdSpec

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

readInterface :: (String -> Bool) -> IO (Either [String] String)
readInterface f = do
  ns <- filter f <$> listInterfaces
  case ns of
    [] -> return $ Left ["no interfaces found"]
    (x:xs) -> do
      unless (null xs) $
        putStrLn $ "WARNING: extra interfaces found, using " ++ x
      return $ Right x

vpnPresent :: IO (Maybe String)
vpnPresent = do
  res <- tryIOError $ readProcessWithExitCode "nmcli" args ""
  -- TODO provide some error messages
  return $ case res of
    (Right (ExitSuccess, out, _)) -> if "vpn" `elem` lines out then Nothing else Just "vpn not found"
    _                             -> Just "puke"
  where
    args = ["-c", "no", "-t", "-f", "TYPE", "c", "show"]

rightPlugins :: Maybe Client -> Maybe Client -> IO [MaybeAction CmdSpec]
rightPlugins sysClient sesClient = mapM evalFeature
  [ getWireless
  , getEthernet sysClient
  , getVPN sysClient
  , getBt sysClient
  , getAlsa
  , getBattery
  , getBl sesClient
  , getCk sesClient
  , getSs sesClient
  , ConstFeature lockCmd
  , ConstFeature dateCmd
  ]

getWireless :: BarFeature
getWireless = Feature
  { ftrDepTree = GenTree (Double wirelessCmd $ readInterface isWireless) []
  , ftrName = "wireless status indicator"
  , ftrWarning = Default
  }

getEthernet :: Maybe Client -> BarFeature
getEthernet client = Feature
  { ftrDepTree = DBusTree action client [devDep] []
  , ftrName = "ethernet status indicator"
  , ftrWarning = Default
  }
  where
    action = Double (\i _ -> ethernetCmd i) (readInterface isEthernet)

getBattery :: BarFeature
getBattery = Feature
  { ftrDepTree = GenTree (Single batteryCmd) [IOTest hasBattery]
  , ftrName = "battery level indicator"
  , ftrWarning = Default
  }

getVPN :: Maybe Client -> BarFeature
getVPN client = Feature
  { ftrDepTree = DBusTree (Single (const vpnCmd)) client [vpnDep] [dp]
  , ftrName = "VPN status indicator"
  , ftrWarning = Default
  }
  where
    dp = IOTest vpnPresent

getBt :: Maybe Client -> BarFeature
getBt client = Feature
  { ftrDepTree = DBusTree (Single (const btCmd)) client [btDep] []
  , ftrName = "bluetooth status indicator"
  , ftrWarning = Default
  }

getAlsa :: BarFeature
getAlsa = Feature
  { ftrDepTree = GenTree (Single alsaCmd) [Executable "alsactl"]
  , ftrName = "volume level indicator"
  , ftrWarning = Default
  }

getBl :: Maybe Client -> BarFeature
getBl client = Feature
  { ftrDepTree = DBusTree (Single (const blCmd)) client [intelBacklightSignalDep] []
  , ftrName = "Intel backlight indicator"
  , ftrWarning = Default
  }

getCk :: Maybe Client -> BarFeature
getCk client = Feature
  { ftrDepTree = DBusTree (Single (const ckCmd)) client [clevoKeyboardSignalDep] []
  , ftrName = "Clevo keyboard indicator"
  , ftrWarning = Default
  }

getSs :: Maybe Client -> BarFeature
getSs client = Feature
  { ftrDepTree = DBusTree (Single (const ssCmd)) client [ssSignalDep] []
  , ftrName = "screensaver indicator"
  , ftrWarning = Default
  }

getAllCommands :: [MaybeAction CmdSpec] -> IO BarRegions
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

data Font = Text
  | IconSmall
  | IconMedium
  | IconLarge
  | IconXLarge
  deriving (Eq, Enum, Bounded, Show)

-- font data ~ (offset, fontification string)
fontData :: Font -> (Int, String)
fontData Text       = (16, barFont)
fontData IconSmall  = (16, nerdFont 13)
fontData IconMedium = (17, nerdFont 15)
fontData IconLarge  = (17, nerdFont 18)
fontData IconXLarge = (18, nerdFont 20)

fontString :: Font -> String
fontString = snd . fontData

fontOffset :: Font -> Int
fontOffset = fst . fontData

allFonts :: [Font]
allFonts = enumFrom minBound

allFontOffsets :: [Int]
allFontOffsets = fontOffset <$> allFonts

allFontStrings :: [String]
allFontStrings = fontString <$> allFonts

barFont :: String
barFont = T.fmtFontXFT T.font
  { T.family = "DejaVu Sans Mono"
  , T.size = Just 11
  , T.weight = Just T.Bold
  }

nerdFont :: Int -> String
nerdFont size = T.fmtFontXFT T.font
  { T.family = "Symbols Nerd Font"
  , T.size = Nothing
  , T.pixelsize = Just size
  }

fontifyText :: Font -> String -> String
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

