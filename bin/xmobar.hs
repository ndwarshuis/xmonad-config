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

import           Control.Monad                                  (unless)
import           Data.Either
import           Data.List
import           Data.Maybe

import           DBus.Client

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
import           XMonad.Hooks.DynamicLog
    ( wrap
    , xmobarColor
    )
import           XMonad.Internal.Command.Power                  (hasBattery)
import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Control
import           XMonad.Internal.Shell
-- import           XMonad.Internal.DBus.Common                    (xmonadBus)
-- import           XMonad.Internal.DBus.Control                   (pathExists)
import           XMonad.Internal.DBus.Screensaver               (ssSignalDep)
import           XMonad.Internal.Dependency
-- import           XMonad.Internal.Shell                          (fmtCmd)
import qualified XMonad.Internal.Theme                          as T
import           Xmobar

main :: IO ()
main = do
  sysClient <- getDBusClient True
  sesClient <- getDBusClient False
  rs <- rightPlugins sysClient sesClient
  warnMissing rs
  cs <- getAllCommands rs
  d <- getXMonadDir
  -- this is needed to see any printed messages
  hFlush stdout
  mapM_ (maybe skip disconnect) [sysClient, sesClient]
  xmobar $ config cs d

config :: BarRegions -> String -> Config
config br confDir = defaultConfig
  { font = barFont
  , additionalFonts = [iconFont, iconFontLarge, iconFontXLarge]
  , textOffset = 16
  , textOffsets = [16, 17, 18]
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
    $ Device (iface, "<fn=2>\xf0e8</fn>", T.fgColor, T.backdropFgColor) 5
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
    , "-o" , "<fn=1>\xf0e7</fn>"
    , "-O" , "<fn=1>\xf1e6</fn>"
    , "-i" , "<fn=1>\xf1e6</fn>"
    ] 50
  }
  where
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
  , csRunnable = Run
    $ VPN ("<fn=2>\xf023</fn>", T.fgColor, T.backdropFgColor) 5
  }

btCmd :: CmdSpec
btCmd = CmdSpec
  { csAlias = btAlias
  , csRunnable = Run
    $ Bluetooth ("<fn=2>\xf293</fn>", T.fgColor, T.backdropFgColor) 5
  }

alsaCmd :: CmdSpec
alsaCmd = CmdSpec
  { csAlias = "alsa:default:Master"
  , csRunnable = Run
    $ Alsa "default" "Master"
    [ "-t", "<status><volume>%"
    , "--"
    , "-O", "<fn=1>\xf028</fn>"
    , "-o", "<fn=1>\xf026 </fn>"
    , "-c", T.fgColor
    , "-C", T.fgColor
    ]
  }

blCmd :: CmdSpec
blCmd = CmdSpec
  { csAlias = blAlias
  , csRunnable = Run $ IntelBacklight "<fn=1>\xf185</fn>"
  }

ckCmd :: CmdSpec
ckCmd = CmdSpec
  { csAlias = ckAlias
  , csRunnable = Run $ ClevoKeyboard "<fn=1>\xf40b</fn>"
  }

ssCmd :: CmdSpec
ssCmd = CmdSpec
  { csAlias = ssAlias
  , csRunnable = Run
    $ Screensaver ("<fn=1>\xf254</fn>", T.fgColor, T.backdropFgColor)
  }

lockCmd :: CmdSpec
lockCmd = CmdSpec
  { csAlias = "locks"
  , csRunnable = Run
    $ Locks
    [ "-N", "<fn=3>\xf8a5</fn>"
    , "-n", xmobarColor T.backdropFgColor "" "<fn=3>\xf8a5</fn>"
    , "-C", "<fn=3>\xf657</fn>"
    , "-c", xmobarColor T.backdropFgColor "" "<fn=3>\xf657</fn>"
    , "-s", ""
    , "-S", ""
    , "-d", " "
    ]
  }

dateCmd :: CmdSpec
dateCmd = CmdSpec
  { csAlias = "date"
  , csRunnable = Run $ Date "%Y-%m-%d %H:%M:%S " "date" 10
  }

--------------------------------------------------------------------------------
-- | command runtime checks and setup

-- some commands depend on the presence of interfaces that can only be
-- determined at runtime; define these checks here

-- in the case of network interfaces, assume that the system uses systemd in
-- which case ethernet interfaces always start with "en" and wireless
-- interfaces always start with "wl"
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
  , getEthernet
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
  { ftrMaybeAction = Chain wirelessCmd $ readInterface isWireless
  , ftrName = "wireless status indicator"
  , ftrWarning = Default
  }

-- TODO this needs a dbus interface
getEthernet :: BarFeature
getEthernet = Feature
  { ftrMaybeAction = Chain ethernetCmd (readInterface isEthernet)
  , ftrName = "ethernet status indicator"
  , ftrWarning = Default
  }

  -- i <- readInterface isEthernet
  -- evalFeature $ maybe BlankFeature (featureDefault "ethernet status indicator" [dep] . ethernetCmd) i
  -- where
  --   dep = dbusDep True devBus devPath devInterface $ Method_ devGetByIP

getBattery :: BarFeature
getBattery = Feature
  { ftrMaybeAction = Parent batteryCmd [IOTest hasBattery]
  , ftrName = "battery level indicator"
  , ftrWarning = Default
  }

type BarFeature = Feature CmdSpec

getVPN :: Maybe Client -> BarFeature
getVPN client = Feature
  { ftrMaybeAction = DBusEndpoint_ (const vpnCmd) client [ep] [dp]
  , ftrName = "VPN status indicator"
  , ftrWarning = Default
  }
  where
    ep = Endpoint vpnBus vpnPath vpnInterface $ Property_ vpnConnType
    dp = IOTest vpnPresent

getBt :: Maybe Client -> BarFeature
getBt client = Feature
  { ftrMaybeAction = DBusEndpoint_ (const btCmd) client [ep] []
  , ftrName = "bluetooth status indicator"
  , ftrWarning = Default
  }
  where
    ep = Endpoint btBus btPath btInterface $ Property_ btPowered

getAlsa :: BarFeature
getAlsa = Feature
  { ftrMaybeAction = Parent alsaCmd [Executable "alsactl"]
  , ftrName = "volume level indicator"
  , ftrWarning = Default
  }

getBl :: Maybe Client -> BarFeature
getBl client = Feature
  { ftrMaybeAction = DBusEndpoint_ (const blCmd) client [intelBacklightSignalDep] []
  , ftrName = "Intel backlight indicator"
  , ftrWarning = Default
  }

getCk :: Maybe Client -> BarFeature
getCk client = Feature
  { ftrMaybeAction = DBusEndpoint_ (const ckCmd) client [clevoKeyboardSignalDep] []
  , ftrName = "Clevo keyboard indicator"
  , ftrWarning = Default
  }

getSs :: Maybe Client -> BarFeature
getSs client = Feature
  { ftrMaybeAction = DBusEndpoint_ (const ssCmd) client [ssSignalDep] []
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
    , brRight = mapMaybe eval right
    }
  where
    eval (Right x) = Just x
    eval _         = Nothing

--------------------------------------------------------------------------------
-- | various formatting things

sep :: String
sep = xmobarColor T.backdropFgColor "" " : "

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

iconFont :: String
iconFont = nerdFont 13

iconFontLarge :: String
iconFontLarge = nerdFont 15

iconFontXLarge :: String
iconFontXLarge = nerdFont 20
