--------------------------------------------------------------------------------
-- | General commands

module XMonad.Internal.Command.Desktop
  ( myTerm
  , runTerm
  , runTMux
  , runCalc
  , runBrowser
  , runEditor
  , runFileManager
  , runTogglePlay
  , runPrevTrack
  , runNextTrack
  , runStopPlay
  , runVolumeDown
  , runVolumeUp
  , runVolumeMute
  , runToggleBluetooth
  , runToggleEthernet
  , runRestart
  , runRecompile
  , runAreaCapture
  , runScreenCapture
  , runDesktopCapture
  , runCaptureBrowser
  , runStartISyncTimer
  , runStartISyncService
  , runNotificationClose
  , runNotificationCloseAll
  , runNotificationHistory
  , runNotificationContext
  , playSound
  ) where

import           Control.Monad              (void)
import           Control.Monad.IO.Class

import           System.Directory
    ( createDirectoryIfMissing
    , getHomeDirectory
    )
import           System.Environment
import           System.FilePath

import           XMonad.Actions.Volume
import           XMonad.Core                hiding (spawn)
import           XMonad.Internal.Dependency
import           XMonad.Internal.Notify
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import           XMonad.Operations

--------------------------------------------------------------------------------
-- | My Executables

myTerm :: String
myTerm = "urxvt"

myBrowser :: String
myBrowser = "brave-accel"

myEditor :: String
myEditor = "emacsclient"

myMultimediaCtl :: String
myMultimediaCtl = "playerctl"

myBluetooth :: String
myBluetooth = "bluetoothctl"

myCapture :: String
myCapture = "flameshot"

myImageBrowser :: String
myImageBrowser = "feh"

myNotificationCtrl :: String
myNotificationCtrl = "dunstctl"

--------------------------------------------------------------------------------
-- | Misc constants

volumeChangeSound :: FilePath
volumeChangeSound = "smb_fireball.wav"

ethernetIface :: String
ethernetIface = "enp7s0f1"

--------------------------------------------------------------------------------
-- | Some nice apps

runTerm :: FeatureX
runTerm = featureSpawn myTerm

runTMux :: FeatureX
runTMux = featureRun [exe myTerm, exe "tmux", exe "bash"] cmd
  where
    cmd = spawn
      $ "tmux has-session"
      #!&& fmtCmd myTerm ["-e", "bash", "-c", singleQuote c]
      #!|| fmtNotifyCmd defNoteError { body = Just $ Text msg }
    c = "exec tmux attach-session -d"
    msg = "could not connect to tmux session"

runCalc :: FeatureX
runCalc = featureRun [exe myTerm, exe "R"] $ spawnCmd myTerm ["-e", "R"]

runBrowser :: FeatureX
runBrowser = featureSpawn myBrowser

runEditor :: FeatureX
runEditor = featureSpawnCmd myEditor
  ["-c", "-e", doubleQuote "(select-frame-set-input-focus (selected-frame))"]

runFileManager :: FeatureX
runFileManager = featureSpawn "pcmanfm"

--------------------------------------------------------------------------------
-- | Multimedia Commands

runMultimediaIfInstalled :: String -> FeatureX
runMultimediaIfInstalled cmd = featureSpawnCmd myMultimediaCtl [cmd]

runTogglePlay :: FeatureX
runTogglePlay = runMultimediaIfInstalled "play-pause"

runPrevTrack :: FeatureX
runPrevTrack = runMultimediaIfInstalled "previous"

runNextTrack :: FeatureX
runNextTrack = runMultimediaIfInstalled "next"

runStopPlay :: FeatureX
runStopPlay = runMultimediaIfInstalled "stop"

--------------------------------------------------------------------------------
-- | Volume Commands

soundDir :: FilePath
soundDir = "sound"

playSound :: MonadIO m => FilePath -> m ()
playSound file = do
  p <- (</> soundDir </> file) <$> getXMonadDir
  -- paplay seems to have less latency than aplay
  spawnCmd "paplay" [p]

featureSound :: FilePath -> X () -> X () -> FeatureX
featureSound file pre post = featureRun [exe "paplay"]
  $ pre >> playSound file >> post

runVolumeDown :: FeatureX
runVolumeDown = featureSound volumeChangeSound (return ()) $ void (lowerVolume 2)

runVolumeUp :: FeatureX
runVolumeUp = featureSound volumeChangeSound (return ()) $ void (raiseVolume 2)

runVolumeMute :: FeatureX
runVolumeMute = featureSound volumeChangeSound (void toggleMute) $ return ()

--------------------------------------------------------------------------------
-- | Notification control

runNotificationCmd :: String -> FeatureX
runNotificationCmd cmd = featureSpawnCmd myNotificationCtrl [cmd]

runNotificationClose :: FeatureX
runNotificationClose = runNotificationCmd "close"

runNotificationCloseAll :: FeatureX
runNotificationCloseAll = runNotificationCmd "close-all"

runNotificationHistory :: FeatureX
runNotificationHistory = runNotificationCmd "history-pop"

runNotificationContext :: FeatureX
runNotificationContext = runNotificationCmd "context"

--------------------------------------------------------------------------------
-- | System commands

runToggleBluetooth :: FeatureX
runToggleBluetooth = featureRun [exe myBluetooth] $ spawn
  $ myBluetooth ++ " show | grep -q \"Powered: no\""
  #!&& "a=on"
  #!|| "a=off"
  #!>> fmtCmd myBluetooth ["power", "$a", ">", "/dev/null"]
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "bluetooth powered $a"  }

runToggleEthernet :: FeatureX
runToggleEthernet = featureRun [exe "nmcli"] $ spawn
  $ "nmcli -g GENERAL.STATE device show " ++ ethernetIface ++ " | grep -q disconnected"
  #!&& "a=connect"
  #!|| "a=disconnect"
  #!>> fmtCmd "nmcli" ["device", "$a", ethernetIface]
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "ethernet \"$a\"ed"  }

runStartISyncTimer :: FeatureX
runStartISyncTimer = featureRun [userUnit "mbsync.timer"]
  $ spawn
  $ "systemctl --user start mbsync.timer"
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "Isync timer started"  }
  #!|| fmtNotifyCmd defNoteError { body = Just $ Text "Isync timer failed to start" }

runStartISyncService :: FeatureX
runStartISyncService = featureRun [userUnit "mbsync.service"]
  $ spawn
  $ "systemctl --user start mbsync.service"
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "Isync completed" }
  #!|| fmtNotifyCmd defNoteError { body = Just $ Text "Isync failed" }

--------------------------------------------------------------------------------
-- | Configuration commands

runRestart :: X ()
runRestart = restart "xmonad" True

runRecompile :: X ()
runRecompile = do
  -- assume that the conf directory contains a valid stack project
  confDir <- getXMonadDir
  spawnAt confDir $ fmtCmd "stack" ["install"]
    #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "compilation succeeded" }
    #!|| fmtNotifyCmd defNoteError { body = Just $ Text "compilation failed" }

-- runRecompile :: X ()
-- runRecompile = do
--   -- assume that the conf directory contains a valid stack project
--   -- TODO this is hacky AF
--   confDir <- getXMonadDir
--   spawnCmdAt confDir "stack" ["install"]

--------------------------------------------------------------------------------
-- | Screen capture commands

getCaptureDir :: IO FilePath
getCaptureDir = do
  e <- lookupEnv "XDG_DATA_HOME"
  parent <- case e of
    Nothing -> fallback
    Just path
      | isRelative path -> fallback
      | otherwise -> return path
  let fullpath = parent </> "screenshots"
  createDirectoryIfMissing True fullpath
  return fullpath
  where
    fallback = (</> ".local/share") <$> getHomeDirectory

runFlameshot :: String -> FeatureX
runFlameshot mode = featureRun [exe myCapture] $ do
  ssDir <- io getCaptureDir
  spawnCmd myCapture $ mode : ["-p", ssDir]

-- TODO this will steal focus from the current window (and puts it
-- in the root window?) ...need to fix
runAreaCapture :: FeatureX
runAreaCapture = runFlameshot "gui"

-- myWindowCap = "screencap -w" --external script

runDesktopCapture :: FeatureX
runDesktopCapture = runFlameshot "full"

runScreenCapture :: FeatureX
runScreenCapture = runFlameshot "screen"

runCaptureBrowser :: FeatureX
runCaptureBrowser = featureRun [exe myImageBrowser] $ do
  dir <- io getCaptureDir
  spawnCmd myImageBrowser [dir]
