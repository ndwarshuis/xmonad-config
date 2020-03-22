import Xmobar.Plugins.Bluetooth
import Xmobar.Plugins.IntelBacklight
import Xmobar.Plugins.NetworkManager
import Xmobar.Plugins.Screensaver

import qualified Theme as T

import Data.List

import Xmobar
import XMonad (getXMonadDir)

wrapColor :: String -> String -> String
wrapColor c s = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

sep :: String
sep = wrapColor T.backdropFgColor " : "

myTemplate :: String
myTemplate = formatTemplate left right
  where
    formatTemplate l r = intercalate sep l
      ++ " }{ "
      ++ intercalate sep r
      ++ " "
    left = [ "%UnsafeStdinReader%" ]
    right = [ "%wlp0s20f3wi%"
            , "%alsa:default:Master%"
            , "%battery%"
            , "%intelbacklight%"
            , "%bluetooth%"
            , "%screensaver%"
            , "%networkmanager%"
            , "%locks%"
            , "%date%"
            ]

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

blockFont :: String
blockFont = T.fmtFontXFT T.font
  { T.family = "Symbola"
  , T.size = Just 13
  , T.weight = Just T.Bold
  }

config :: String -> Config
config confDir = defaultConfig
  { font = barFont
  , additionalFonts = [ iconFont, blockFont ]
  , textOffset = 16
  , textOffsets = [ 16, 17 ]
  , bgColor = T.bgColor
  , fgColor = T.fgColor
  , position = BottomSize C 100 24
  , border = NoBorder
  , borderColor = T.bordersColor

  , sepChar = "%"
  , alignSep = "}{"
  , template = myTemplate

  , lowerOnStart = False
  , hideOnStart = False
  , allDesktops = True
  , overrideRedirect = True
  , pickBroadest = False
  , persistent = True
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

      , Run $ Locks
        [ "-N", "<fn=2>\x1f13d</fn>"
        , "-n", wrapColor T.backdropFgColor "<fn=2>\x1f13d</fn>"
        , "-C", "<fn=2>\x1f132</fn>"
        , "-c", wrapColor T.backdropFgColor "<fn=2>\x1f132</fn>"
        , "-s", ""
        , "-S", ""
        , "-d", "<fn=2> </fn>"
        ]

      , Run $ Date "%Y-%m-%d %H:%M" "date" 10

      , Run $ Screensaver ("<fn=1>\xf254</fn>", T.fgColor, T.backdropFgColor)

      , Run $ Bluetooth ("<fn=1>\xf293</fn>", T.fgColor, T.backdropFgColor) 5

      , Run UnsafeStdinReader

      , Run $ NetworkManager ("VPN", T.fgColor, T.backdropFgColor) 5
      ]
  }

main :: IO ()
main = do
  confDir <- getXMonadDir
  xmobar $ config confDir
