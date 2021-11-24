
module Xmobar.Plugins.Common
  (chooseColor)
  where

import           XMonad.Hooks.DynamicLog (xmobarColor)

chooseColor :: String -> String -> String -> Bool -> String
chooseColor text colorOn colorOff state =
  xmobarColor (if state then colorOn else colorOff) "" text
