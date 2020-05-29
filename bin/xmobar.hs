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

import           Xmobar.Plugins.Bluetooth
import           Xmobar.Plugins.Device
import           Xmobar.Plugins.IntelBacklight
import           Xmobar.Plugins.Screensaver
import           Xmobar.Plugins.VPN

import           Xmobar
import           XMonad                        (getXMonadDir)
import           XMonad.Hooks.DynamicLog       (wrap, xmobarColor)
import qualified XMonad.Internal.Theme         as T

sep :: String
sep = xmobarColor T.backdropFgColor "" " : "

aSep :: String
aSep = "}{"

pSep :: String
pSep = "%"

myTemplate :: String
myTemplate = formatTemplate left right
  where
    formatTemplate l r = fmtAliases l ++ aSep ++ fmtAliases r ++ " "
    left = [ "UnsafeStdinReader" ]
    right = [ "wlp0s20f3wi"
            , "enp7s0f1"
            , "vpn"
            , "bluetooth"
            , "alsa:default:Master"
            , "battery"
            , "intelbacklight"
            , "screensaver"
            , "locks"
            , "date"
            ]
    fmtAliases = intercalate sep . map (wrap pSep pSep)

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

config :: String -> Config
config confDir = defaultConfig
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
  , alignSep = aSep
  , template = myTemplate

  , lowerOnStart = False
  , hideOnStart = False
  , allDesktops = True
  , overrideRedirect = True
  , pickBroadest = False
  , persistent = True
  -- store the icons with the xmonad/xmobar stack project
  , iconRoot = confDir ++ "/icons"

  , commands =
      [ Run $ Alsa "default" "Master"
        [ "-t", "<status><volume>%"
        , "--"
        , "-O", "<fn=1>\xf028</fn>"
        , "-o", "<fn=1>\xf026 </fn>"
        , "-c", T.fgColor
        , "-C", T.fgColor
        ]

      , Run $ Battery [ "--template", "<acstatus><left>"
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

      , Run $ IntelBacklight "<fn=1>\xf185</fn>"

      , Run $ Wireless "wlp0s20f3"
        [ "-t", "<qualityipat><essid>"
        , "--"
        , "--quality-icon-pattern", "<icon=wifi_%%.xpm/>"
        ] 5

      , Run $ Device
        ("enp7s0f1", "<fn=2>\xf0e8</fn>", T.fgColor, T.backdropFgColor) 5

      , Run $ Locks
        [ "-N", "<fn=3>\x1f13d</fn>"
        , "-n", xmobarColor T.backdropFgColor "" "<fn=3>\x1f13d</fn>"
        , "-C", "<fn=3>\x1f132</fn>"
        , "-c", xmobarColor T.backdropFgColor "" "<fn=3>\x1f132</fn>"
        , "-s", ""
        , "-S", ""
        , "-d", "<fn=3> </fn>"
        ]

      , Run $ Date "%Y-%m-%d %H:%M" "date" 10

      , Run $ Screensaver ("<fn=1>\xf254</fn>", T.fgColor, T.backdropFgColor)

      , Run $ Bluetooth ("<fn=2>\xf293</fn>", T.fgColor, T.backdropFgColor) 5

      , Run UnsafeStdinReader

      , Run $ VPN ("<fn=2>\xf023</fn>", T.fgColor, T.backdropFgColor) 5
      ]
  }

main :: IO ()
main = xmobar =<< config <$> getXMonadDir
