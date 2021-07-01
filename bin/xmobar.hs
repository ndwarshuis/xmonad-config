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

import           DBus

import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process                      (readProcessWithExitCode)

import           Xmobar.Plugins.Bluetooth
import           Xmobar.Plugins.Device
import           Xmobar.Plugins.IntelBacklight
import           Xmobar.Plugins.Screensaver
import           Xmobar.Plugins.VPN

import           XMonad                              (getXMonadDir)
import           XMonad.Hooks.DynamicLog             (wrap, xmobarColor)
import           XMonad.Internal.Command.Power       (hasBattery)
import           XMonad.Internal.DBus.Common         (xmonadBus)
import           XMonad.Internal.DBus.Control        (pathExists)
import           XMonad.Internal.DBus.IntelBacklight (blPath)
import           XMonad.Internal.DBus.Screensaver    (ssPath)
import qualified XMonad.Internal.Theme               as T
import           Xmobar

main :: IO ()
main = do
  cs <- getAllCommands
  d <- getXMonadDir
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
    , "-P"
    , "-o" , "<fn=1>\xf0e7</fn>"
    , "-O" , "<fn=1>\xf1e6</fn>"
    , "-i" , "<fn=1>\xf1e6</fn>"
    ] 50
  }

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

noSetup :: Monad m => a -> m (Maybe a)
noSetup = return . Just

toJust :: a -> Bool -> Maybe a
toJust x b = if b then Just x else Nothing

whenDBusPath :: Bool -> BusName -> ObjectPath -> CmdSpec -> IO (Maybe CmdSpec)
whenDBusPath usesys b p cs = toJust cs <$> pathExists usesys b p

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

getWireless :: IO (Maybe CmdSpec)
getWireless = do
  ns <- filter isWireless <$> listInterfaces
  return $ case ns of
    [n] -> Just $ wirelessCmd n
    _   -> Nothing

getEthernet :: IO (Maybe CmdSpec)
getEthernet = do
  e <- pathExists True devBus devPath
  ns <- filter isEthernet <$> listInterfaces
  return $ case ns of
    [n] -> toJust (ethernetCmd n) e
    _   -> Nothing

getBattery :: IO (Maybe CmdSpec)
getBattery = toJust batteryCmd <$> hasBattery

getVPN :: IO (Maybe CmdSpec)
getVPN = do
  res <- tryIOError $ readProcessWithExitCode "nmcli" args ""
  case res of
    (Right (ExitSuccess, out, _)) -> do
      e <- pathExists True vpnBus vpnPath
      return $ toJust vpnCmd (e && "vpn" `elem` lines out)
    (Left _)                      -> do
      putStrLn "WARNING: could not get list of network interfaces"
      return Nothing
    _                             -> return Nothing
  where
    args = ["-c", "no", "-t", "-f", "TYPE", "c", "show"]

getBt :: IO (Maybe CmdSpec)
getBt = whenDBusPath True btBus btPath btCmd

getAlsa :: IO (Maybe CmdSpec)
getAlsa = toJust alsaCmd . isJust <$> findExecutable "alsactl"

getBl :: IO (Maybe CmdSpec)
getBl = whenDBusPath False xmonadBus blPath blCmd

getSs :: IO (Maybe CmdSpec)
getSs = whenDBusPath False xmonadBus ssPath ssCmd

getAllCommands :: IO BarRegions
getAllCommands = do
  let left =
        [ CmdSpec
          { csAlias = "UnsafeStdinReader"
          , csRunnable = Run UnsafeStdinReader
          }
        ]
  right <- catMaybes <$> sequence
    [ getWireless
    , getEthernet
    , getVPN
    , getBt
    , getAlsa
    , getBattery
    , getBl
    , getSs
    , noSetup lockCmd
    , noSetup dateCmd
    ]
  -- this is needed to see any printed messages
  hFlush stdout
  return $ BarRegions
    { brLeft = left
    , brCenter = []
    , brRight = right
    }

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

iconFont :: String
iconFont = T.fmtFontXFT T.font
  { T.family = "Symbols Nerd Font"
  , T.size = Nothing
  , T.pixelsize = Just 13
  }

iconFontLarge :: String
iconFontLarge = T.fmtFontXFT T.font
  { T.family = "Symbols Nerd Font"
  , T.size = Nothing
  , T.pixelsize = Just 15
  }

iconFontXLarge :: String
iconFontXLarge = T.fmtFontXFT T.font
  { T.family = "Symbols Nerd Font"
  , T.size = Nothing
  , T.pixelsize = Just 20
  }
