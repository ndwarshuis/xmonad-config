module Xmobar.Screensaver where

-- import Control.Monad

import Graphics.X11.Xlib.Display
import Graphics.X11.XScreenSaver

import Xmobar

data Screensaver = Screensaver (String, String, String) Int
  deriving (Read, Show)

instance Exec Screensaver where
    alias (Screensaver _ _) = "screensaver"
    run   (Screensaver opts _) = run' opts
    rate  (Screensaver _ r) = r

-- TODO make this respond to events rather than polling
run' :: (String, String, String) -> IO String
run' (text, colorOn, colorOff) = do
  dpy <- openDisplay ""
  xssi <- xScreenSaverQueryInfo dpy
  print "hi"
  closeDisplay dpy
  return $ case xssi of
    Just x -> wrapColor text
      $ case xssi_state x of
          ScreenSaverDisabled -> colorOff
          _ -> colorOn
    Nothing -> "N/A"
  where
    -- TODO not DRY
    wrapColor s c = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"
  
