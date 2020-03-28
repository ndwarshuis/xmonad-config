module Capture
  ( runAreaCapture
  , runDesktopCapture
  , runScreenCapture
  )
where

import           Shell

import           System.Directory

import           XMonad.Core

getScreenshotDir :: IO FilePath
getScreenshotDir = do
  h <- getHomeDirectory
  return $ h ++ "/Pictures/screenshots"

runFlameshot :: String -> X ()
runFlameshot mode = do
  ssDir <- io getScreenshotDir
  spawnCmd "flameshot" $ mode : ["-p", ssDir]

-- TODO this will steal focus from the current window (and puts it
-- in the root window?) ...need to fix
runAreaCapture :: X ()
runAreaCapture = runFlameshot "gui"

-- myWindowCap = "screencap -w" --external script

runScreenCapture :: X ()
runScreenCapture = runFlameshot "screen"

runDesktopCapture :: X ()
runDesktopCapture = runFlameshot "full"
