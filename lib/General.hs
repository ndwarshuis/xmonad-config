module General where

--------------------------------------------------------------------------------
-- | General commands

import           Notify
import           Shell

import           DBus.IntelBacklight
import           DBus.Screensaver

import           Control.Monad         (void)

import           XMonad.Actions.Volume
import           XMonad.Core
import           XMonad.Operations

--------------------------------------------------------------------------------
-- | Some nice apps

runTerm :: X ()
runTerm = spawn myTerm

runCalc :: X ()
runCalc = spawnCmd myTerm ["-e", "R"]

runBrowser :: X ()
runBrowser = spawn "brave"

runEditor :: X ()
runEditor = spawnCmd "emacsclient"
  ["-c", "-e", "\"(select-frame-set-input-focus (selected-frame))\""]

runFileManager :: X ()
runFileManager = spawn "pcmanfm"

--------------------------------------------------------------------------------
-- | Multimedia Commands

myMultimediaCtl :: String
myMultimediaCtl = "playerctl"

runTogglePlay :: X ()
runTogglePlay = spawnCmd myMultimediaCtl ["play-pause"]

runPrevTrack :: X ()
runPrevTrack = spawnCmd myMultimediaCtl ["previous"]

runNextTrack :: X ()
runNextTrack = spawnCmd myMultimediaCtl ["next"]

runStopPlay :: X ()
runStopPlay = spawnCmd myMultimediaCtl ["stop"]

runVolumeDown :: X ()
runVolumeDown = void (lowerVolume 2)

runVolumeUp :: X ()
runVolumeUp = void (raiseVolume 2)

runVolumeMute :: X ()
runVolumeMute = void toggleMute

--------------------------------------------------------------------------------
-- | System commands

runToggleBluetooth :: X ()
runToggleBluetooth = spawn
  $ "bluetoothctl show | grep -q \"Powered: no\""
  #!&& "a=on"
  #!|| "a=off"
  #!>> fmtCmd "bluetoothctl" ["power", "$a", ">", "/dev/null"]
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "bluetooth powered $a"  }

runIncBacklight :: X ()
runIncBacklight = io $ void callIncBrightness

runDecBacklight :: X ()
runDecBacklight = io $ void callDecBrightness

runMinBacklight :: X ()
runMinBacklight = io $ void callMinBrightness

runMaxBacklight :: X ()
runMaxBacklight = io $ void callMaxBrightness

runToggleDPMS :: X ()
runToggleDPMS = io $ void callToggle

--------------------------------------------------------------------------------
-- | Configuration commands

runRestart :: X ()
runRestart = restart "xmonad" True

runRecompile :: X ()
runRecompile = do
  -- assume that the conf directory contains a valid stack project
  -- TODO this is hacky AF
  confDir <- getXMonadDir
  spawn $ cmd confDir
  where
    cmd c = fmtCmd "cd" [c]
      #!&& fmtCmd "stack" ["install", ":xmonad"]
      #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "compilation succeeded" }
      #!|| fmtNotifyCmd defNoteError { body = Just $ Text "compilation failed" }
