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

import           DBus

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
import           XMonad.Internal.DBus.Brightness.IntelBacklight
    ( curFileDep
    , maxFileDep
    )
-- import           XMonad.Internal.DBus.Common                    (xmonadBus)
-- import           XMonad.Internal.DBus.Control                   (pathExists)
import           XMonad.Internal.DBus.Screensaver               (ssDep)
import           XMonad.Internal.Dependency
-- import           XMonad.Internal.Shell                          (fmtCmd)
import qualified XMonad.Internal.Theme                          as T
import           Xmobar

main :: IO ()
main = do
  rs <- sequence rightPlugins
  warnMissing rs
  cs <- getAllCommands rs
  d <- getXMonadDir
  -- this is needed to see any printed messages
  hFlush stdout
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
  { csAlias = "intelbacklight"
  , csRunnable = Run $ IntelBacklight "<fn=1>\xf185</fn>"
  }

ckCmd :: CmdSpec
ckCmd = CmdSpec
  { csAlias = ckAlias
  , csRunnable = Run $ ClevoKeyboard ("<fn=1>\xf40b</fn>", T.fgColor, T.backdropFgColor)  5
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

-- noSetup :: Monad m => a -> m (Maybe a)
-- noSetup = return . Just

-- toJust :: a -> Bool -> Maybe a
-- toJust x b = if b then Just x else Nothing

dbusDep :: Bool -> BusName -> ObjectPath -> InterfaceName -> DBusMember -> Dependency a
dbusDep usesys bus obj iface mem =
  Dependency { depRequired = True, depData = d }
  where
    d = DBusEndpoint
      { ddDbusBus = bus
      , ddDbusSystem = usesys
      , ddDbusObject = obj
      , ddDbusInterface = iface
      , ddDbusMember = mem
      }

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

readInterface :: (String -> Bool) -> IO (Maybe String)
readInterface f = do
  ns <- filter f <$> listInterfaces
  case ns of
    (x:xs) -> do
      unless (null xs) $
        putStrLn $ "WARNING: extra interfaces found, using " ++ x
      return $ Just x
    _     -> return Nothing

vpnPresent :: IO (Either String Bool)
vpnPresent = do
  res <- tryIOError $ readProcessWithExitCode "nmcli" args ""
  -- TODO provide some error messages
  return $ case res of
    (Right (ExitSuccess, out, _)) -> Right $ "vpn" `elem` lines out
    _                             -> Left "puke"
  where
    args = ["-c", "no", "-t", "-f", "TYPE", "c", "show"]

rightPlugins :: [IO (MaybeExe CmdSpec)]
rightPlugins =
  [ getWireless
  , getEthernet
  , evalFeature getVPN
  , evalFeature getBt
  , evalFeature getAlsa
  , evalFeature getBattery
  , evalFeature getBl
  , nocheck ckCmd
  , evalFeature getSs
  , nocheck lockCmd
  , nocheck dateCmd
  ]
  where
    nocheck = return . flip Installed []

getWireless :: IO (MaybeExe CmdSpec)
getWireless = do
  i <- readInterface isWireless
  return $ maybe Ignore (flip Installed [] . wirelessCmd) i

getEthernet :: IO (MaybeExe CmdSpec)
getEthernet = do
  i <- readInterface isEthernet
  maybe (return Ignore) (runIfInstalled [dep] . ethernetCmd) i
  where
    dep = dbusDep True devBus devPath devInterface $ Method_ devGetByIP

getBattery :: BarFeature
getBattery = Feature
  { ftrAction = batteryCmd
  , ftrSilent = False
  , ftrDefault = Nothing
  , ftrChildren = [Dependency { depRequired = True, depData = IOTest hasBattery }]
  }

type BarFeature = Feature CmdSpec (IO ())

getVPN :: BarFeature
getVPN = Feature
  { ftrAction = vpnCmd
  , ftrSilent = False
  , ftrDefault = Nothing
  , ftrChildren = [d, v]
  }
  where
    d = dbusDep True vpnBus vpnPath vpnInterface $ Property_ vpnConnType
    v = Dependency { depRequired = True, depData = IOTest vpnPresent }

getBt :: BarFeature
getBt = Feature
  { ftrAction = btCmd
  , ftrSilent = False
  , ftrDefault = Nothing
  , ftrChildren = [dep]
  }
  where
    dep = dbusDep True btBus btPath btInterface $ Property_ btPowered

getAlsa :: BarFeature
getAlsa = Feature
  { ftrAction = alsaCmd
  , ftrDefault = Nothing
  , ftrSilent = False
  , ftrChildren = [exe "alsactl"]
  }

getBl :: BarFeature
getBl = Feature
  { ftrAction = blCmd
  , ftrDefault = Nothing
  , ftrSilent = False
  , ftrChildren = [curFileDep, maxFileDep]
  }

getSs :: BarFeature
getSs = Feature
  { ftrAction = ssCmd
  , ftrDefault = Nothing
  , ftrSilent = False
  , ftrChildren = [ssDep]
  }

getAllCommands :: [MaybeExe CmdSpec] -> IO BarRegions
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
    eval (Installed x _) = Just x
    eval _               = Nothing

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
