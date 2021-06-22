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

import           Control.Monad                       (filterM)

import           Data.List

import           DBus

import           Xmobar.Plugins.Bluetooth
import           Xmobar.Plugins.Device
import           Xmobar.Plugins.IntelBacklight
import           Xmobar.Plugins.Screensaver
import           Xmobar.Plugins.VPN

import           XMonad                              (getXMonadDir)
import           XMonad.Hooks.DynamicLog             (wrap, xmobarColor)
import           XMonad.Internal.DBus.Common         (xmonadBus)
import           XMonad.Internal.DBus.Control        (pathExists)
import           XMonad.Internal.DBus.IntelBacklight (blPath)
import           XMonad.Internal.DBus.Screensaver    (ssPath)
import qualified XMonad.Internal.Theme               as T
import           Xmobar

sep :: String
sep = xmobarColor T.backdropFgColor "" " : "

lSep :: Char
lSep = '}'

rSep :: Char
rSep = '{'

pSep :: String
pSep = "%"

data BarRegions = BarRegions
  { brLeft   :: [CmdSpec]
  , brCenter :: [CmdSpec]
  , brRight  :: [CmdSpec]
  } deriving Show

data CmdSpec = CmdSpec
  { csAlias    :: String
  , csDepends  :: Maybe DBusDepends
  , csRunnable :: Runnable
  } deriving Show

data DBusDepends = DBusDepends
  { ddBus  :: BusName
  , ddPath :: ObjectPath
  , ddSys  :: Bool
  } deriving Show

sysDepends :: BusName -> ObjectPath -> DBusDepends
sysDepends b p = DBusDepends b p True

sesDepends :: BusName -> ObjectPath -> DBusDepends
sesDepends b p = DBusDepends b p False

concatRegions :: BarRegions -> [CmdSpec]
concatRegions (BarRegions l c r) = l ++ c ++ r

mapRegionsM :: Monad m => ([CmdSpec] -> m [CmdSpec]) -> BarRegions -> m BarRegions
mapRegionsM f (BarRegions l c r) = do
  l' <- f l
  c' <- f c
  r' <- f r
  return $ BarRegions l' c' r'

filterSpecs :: [CmdSpec] -> IO [CmdSpec]
filterSpecs = filterM (maybe (return True) exists . csDepends)
  where
    exists DBusDepends { ddBus = b, ddPath = p, ddSys = s } = pathExists s b p

myCommands :: BarRegions
myCommands = BarRegions
  { brLeft =
    [ CmdSpec
      { csAlias = "UnsafeStdinReader"
      , csDepends = Nothing
      , csRunnable = Run UnsafeStdinReader
      }
    ]

  , brCenter = []

  , brRight =
    [ CmdSpec
      { csAlias = "wlp0s20f3wi"
      , csDepends = Nothing
      , csRunnable = Run
        $ Wireless "wlp0s20f3"
        [ "-t", "<qualityipat><essid>"
        , "--"
        , "--quality-icon-pattern", "<icon=wifi_%%.xpm/>"
        ] 5
      }

    , CmdSpec
      { csAlias = "enp7s0f1"
      , csDepends = Just $ sysDepends devBus devPath
      , csRunnable = Run
        $ Device ("enp7s0f1", "<fn=2>\xf0e8</fn>", T.fgColor, T.backdropFgColor) 5
      }

    , CmdSpec
      { csAlias = vpnAlias
      , csDepends = Just $ sysDepends vpnBus vpnPath
      , csRunnable = Run
        $ VPN ("<fn=2>\xf023</fn>", T.fgColor, T.backdropFgColor) 5
      }

    , CmdSpec
      { csAlias = btAlias
      , csDepends = Just $ sysDepends btBus btPath
      , csRunnable = Run
        $ Bluetooth ("<fn=2>\xf293</fn>", T.fgColor, T.backdropFgColor) 5
      }

    , CmdSpec
      { csAlias = "alsa:default:Master"
      , csDepends = Nothing
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

    , CmdSpec
      { csAlias = "battery"
      , csDepends = Nothing
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

    , CmdSpec
      { csAlias = "intelbacklight"
      , csDepends = Just $ sesDepends xmonadBus blPath
      , csRunnable = Run $ IntelBacklight "<fn=1>\xf185</fn>"
      }

    , CmdSpec
      { csAlias = ssAlias
      , csDepends = Just $ sesDepends xmonadBus ssPath
      , csRunnable = Run
        $ Screensaver ("<fn=1>\xf254</fn>", T.fgColor, T.backdropFgColor)
      }

    , CmdSpec
      { csAlias = "locks"
      , csDepends = Nothing
      , csRunnable = Run
        $ Locks
        [ "-N", "<fn=3>\x1f13d</fn>"
        , "-n", xmobarColor T.backdropFgColor "" "<fn=3>\x1f13d</fn>"
        , "-C", "<fn=3>\x1f132</fn>"
        , "-c", xmobarColor T.backdropFgColor "" "<fn=3>\x1f132</fn>"
        , "-s", ""
        , "-S", ""
        , "-d", "<fn=3> </fn>"
        ]
      }

    , CmdSpec
      { csAlias = "date"
      , csDepends = Nothing
      , csRunnable = Run $ Date "%Y-%m-%d %H:%M:%S " "date" 10
      }
    ]
  }

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
  { T.family = "FontAwesome"
  , T.size = Nothing
  , T.pixelsize = Just 13
  }

iconFontLarge :: String
iconFontLarge = T.fmtFontXFT T.font
  { T.family = "FontAwesome"
  , T.size = Nothing
  , T.pixelsize = Just 15
  }

blockFont :: String
blockFont = T.fmtFontXFT T.font
  { T.family = "Symbola"
  , T.size = Just 13
  , T.weight = Just T.Bold
  }

config :: BarRegions -> String -> Config
config br confDir = defaultConfig
  { font = barFont
  , additionalFonts = [ iconFont, iconFontLarge, blockFont ]
  , textOffset = 16
  , textOffsets = [ 16, 17, 17 ]
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

main :: IO ()
main = do
  br <- mapRegionsM filterSpecs myCommands
  dir <- getXMonadDir
  xmobar $ config br dir
