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
  , runIncBacklight
  , runDecBacklight
  , runMinBacklight
  , runMaxBacklight
  , runToggleDPMS
  , runToggleEthernet
  , runRestart
  , runRecompile
  , runAreaCapture
  , runScreenCapture
  , runDesktopCapture
  , runCaptureBrowser
  , runStartISyncTimer
  , runStartISyncService
  ) where

import           Control.Monad                       (void)

import           System.Directory
    ( createDirectoryIfMissing
    , getHomeDirectory
    )
import           System.Environment
import           System.FilePath

import           XMonad.Actions.Volume
import           XMonad.Core                         hiding (spawn)
import           XMonad.Internal.DBus.IntelBacklight
import           XMonad.Internal.DBus.Screensaver
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

--------------------------------------------------------------------------------
-- | Misc constants

volumeChangeSound :: FilePath
volumeChangeSound = "smb_fireball.wav"

ethernetIface :: String
ethernetIface = "enp7s0f1"

--------------------------------------------------------------------------------
-- | Some nice apps

runTerm :: IO MaybeX
runTerm = spawnIfInstalled myTerm

runTMux :: IO MaybeX
runTMux = runIfInstalled [exe myTerm, exe "tmux", exe "bash"] cmd
  where
    cmd = spawn
      $ "tmux has-session"
      #!&& fmtCmd myTerm ["-e", "bash", "-c", singleQuote c]
      #!|| fmtNotifyCmd defNoteError { body = Just $ Text msg }
    c = "exec tmux attach-session -d"
    msg = "could not connect to tmux session"

runCalc :: IO MaybeX
runCalc = runIfInstalled [exe myTerm, exe "R"] $ spawnCmd myTerm ["-e", "R"]

runBrowser :: IO MaybeX
runBrowser = spawnIfInstalled myBrowser

runEditor :: IO MaybeX
runEditor = spawnCmdIfInstalled myEditor
  ["-c", "-e", doubleQuote "(select-frame-set-input-focus (selected-frame))"]

runFileManager :: IO MaybeX
runFileManager = spawnIfInstalled "pcmanfm"

--------------------------------------------------------------------------------
-- | Multimedia Commands

runMultimediaIfInstalled :: String -> IO MaybeX
runMultimediaIfInstalled cmd = spawnCmdIfInstalled myMultimediaCtl [cmd]

runTogglePlay :: IO MaybeX
runTogglePlay = runMultimediaIfInstalled "play-pause"

runPrevTrack :: IO MaybeX
runPrevTrack = runMultimediaIfInstalled "previous"

runNextTrack :: IO MaybeX
runNextTrack = runMultimediaIfInstalled "next"

runStopPlay :: IO MaybeX
runStopPlay = runMultimediaIfInstalled "stop"

runVolumeDown :: IO MaybeX
runVolumeDown =  spawnSound volumeChangeSound (return ()) $ void (lowerVolume 2)

runVolumeUp :: IO MaybeX
runVolumeUp = spawnSound volumeChangeSound (return ()) $ void (raiseVolume 2)

runVolumeMute :: IO MaybeX
runVolumeMute = spawnSound volumeChangeSound (void toggleMute) $ return ()

--------------------------------------------------------------------------------
-- | System commands

runToggleBluetooth :: IO MaybeX
runToggleBluetooth = runIfInstalled [exe myBluetooth] $ spawn
  $ myBluetooth ++ " show | grep -q \"Powered: no\""
  #!&& "a=on"
  #!|| "a=off"
  #!>> fmtCmd myBluetooth ["power", "$a", ">", "/dev/null"]
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

runToggleEthernet :: IO MaybeX
runToggleEthernet = runIfInstalled [exe "nmcli"] $ spawn
  $ "nmcli -g GENERAL.STATE device show " ++ ethernetIface ++ " | grep -q disconnected"
  #!&& "a=connect"
  #!|| "a=disconnect"
  #!>> fmtCmd "nmcli" ["device", "$a", ethernetIface]
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "ethernet \"$a\"ed"  }

runStartISyncTimer :: IO MaybeX
runStartISyncTimer = runIfInstalled [userUnit "mbsync.timer"]
  $ spawn
  $ "systemctl --user start mbsync.timer"
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "Isync timer started"  }
  #!|| fmtNotifyCmd defNoteError { body = Just $ Text "Isync timer failed to start" }

runStartISyncService :: IO MaybeX
runStartISyncService = runIfInstalled [userUnit "mbsync.service"]
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

runFlameshot :: String -> IO MaybeX
runFlameshot mode = runIfInstalled [exe myCapture] $ do
  ssDir <- io getCaptureDir
  spawnCmd myCapture $ mode : ["-p", ssDir]

-- TODO this will steal focus from the current window (and puts it
-- in the root window?) ...need to fix
runAreaCapture :: IO MaybeX
runAreaCapture = runFlameshot "gui"

-- myWindowCap = "screencap -w" --external script

runDesktopCapture :: IO MaybeX
runDesktopCapture = runFlameshot "full"

runScreenCapture :: IO MaybeX
runScreenCapture = runFlameshot "screen"

runCaptureBrowser :: IO MaybeX
runCaptureBrowser = runIfInstalled [exe myImageBrowser] $ do
  dir <- io getCaptureDir
  spawnCmd myImageBrowser [dir]
