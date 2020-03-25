module Xmobar.Common where

import           XMonad.Hooks.DynamicLog (xmobarColor)

wrapColor :: String -> String -> String
wrapColor fg = wrapColorBg fg ""

wrapColorBg :: String -> String -> String -> String
wrapColorBg = xmobarColor
