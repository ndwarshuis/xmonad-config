import Xmobar.Screensaver

import Xmobar
import XMonad (getXMonadDir)

fgColor0 :: String
fgColor0 = "black"

fgColor1 :: String
fgColor1 = "#888888"

bgColor0 :: String
bgColor0 = "#eeeeee"

bdColor :: String
bdColor = "#cccccc"

wrapColor :: String -> String -> String
wrapColor c s = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

sep :: String
sep = wrapColor fgColor1 " : "

myTemplate :: String
myTemplate = concat
  [ "%UnsafeStdinReader%"
  , " }{ "
  , "%screensaver%"
  , sep, "%wlp0s20f3wi%"
  , sep, "%alsa:default:Master%"
  , sep, "%battery%"
  , sep, "%bright%"
  , sep, "%locks%"
  , sep, "%date% "
  ] 

config :: String -> Config
config confDir = defaultConfig { 
  font = "xft:DejaVu Sans Mono:size=11:bold:antialias=true"
  , additionalFonts =
    [ "xft:FontAwesome:pixelsize=13:antialias=true:hinting=true"
    , "xft:Symbola:size=13:bold:antialias=true"
    ]
  , textOffset = 16
  , textOffsets = [ 16, 17 ]
  , bgColor = bgColor0
  , fgColor = fgColor0
  , position = BottomSize C 100 24
  , border = NoBorder
  , borderColor = bdColor

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
        , "-c", fgColor0
        , "-C", fgColor0
        ]

      , Run $ Battery [ "--template", "<acstatus><left>"
                    , "--Low", "10"
                    , "--High", "80"
                    , "--low", "red"
                    , "--normal", fgColor0
                    , "--high", fgColor0
                    , "--"
                    , "-P"
                    , "-o" , "<fn=1>\xf0e7</fn>"
                    , "-O" , "<fn=1>\xf1e6</fn>"
                    , "-i" , "<fn=1>\xf1e6</fn>"
                    ] 50

      , Run $ Brightness ["-t", "<fn=1>\xf185</fn><percent>%"
                       , "--"
                       , "-D", "intel_backlight"
                       ] 10

      , Run $ Wireless "wlp0s20f3"
        [ "-t", "<qualityipat><essid>"
        , "--"
        , "--quality-icon-pattern", "<icon=wifi_%%.xpm/>"
        ] 5

      , Run $ Locks
        [ "-N", "<fn=2>\x1f13d</fn>"
        , "-n", wrapColor fgColor1 "<fn=2>\x1f13d</fn>"
        , "-C", "<fn=2>\x1f132</fn>"
        , "-c", wrapColor fgColor1 "<fn=2>\x1f132</fn>"
        , "-s", ""
        , "-S", ""
        , "-d", "<fn=2> </fn>"
        ]

      , Run $ Date "%Y-%m-%d %H:%M" "date" 10

      , Run $ Screensaver ("<fn=1>\xf108</fn>", fgColor0, fgColor1) 10

      , Run UnsafeStdinReader
      ]
  }

main :: IO ()
main = do
  confDir <- getXMonadDir
  xmobar $ config confDir
