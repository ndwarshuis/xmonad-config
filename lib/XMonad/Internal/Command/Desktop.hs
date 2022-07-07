--------------------------------------------------------------------------------
-- | General commands

module XMonad.Internal.Command.Desktop
  ( myTerm
  , playSound

  -- commands
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

  -- daemons
  , runNetAppDaemon
  ) where

import           Control.Monad               (void)
import           Control.Monad.IO.Class

import           DBus.Client

import           System.Directory
    ( createDirectoryIfMissing
    , getHomeDirectory
    )
import           System.Environment
import           System.FilePath

import           XMonad                      (asks)
import           XMonad.Actions.Volume
import           XMonad.Core                 hiding (spawn)
import           XMonad.Internal.DBus.Common
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

--------------------------------------------------------------------------------
-- | Some nice apps

runTerm :: SometimesX
runTerm = sometimesExe "terminal" "urxvt" True myTerm

-- TODO test that tmux is actually running (/tmp/tmux-<UID>/default)
runTMux :: SometimesX
runTMux = sometimesIO_ "terminal multiplexer" "tmux" deps act
  where
    deps = listToAnds (sysExe myTerm) $ fmap sysExe ["tmux", "bash"]
    act = spawn
      $ "tmux has-session"
      #!&& fmtCmd myTerm ["-e", "bash", "-c", singleQuote c]
      #!|| fmtNotifyCmd defNoteError { body = Just $ Text msg }
    c = "exec tmux attach-session -d"
    msg = "could not connect to tmux session"

runCalc :: SometimesX
runCalc = sometimesIO_ "calculator" "R" deps act
  where
    deps = toAnd_ (sysExe myTerm) (sysExe "R")
    act = spawnCmd myTerm ["-e", "R"]

runBrowser :: SometimesX
runBrowser = sometimesExe "web browser" "brave" False myBrowser

-- TODO test that emacs is actually running (/run/user/1000/emacs/server)
runEditor :: SometimesX
runEditor = sometimesExeArgs "text editor" "emacs" True myEditor
  ["-c", "-e", doubleQuote "(select-frame-set-input-focus (selected-frame))"]

runFileManager :: SometimesX
runFileManager = sometimesExe "file browser" "pcmanfm" True "pcmanfm"

--------------------------------------------------------------------------------
-- | Multimedia Commands

runMultimediaIfInstalled :: String -> String -> SometimesX
runMultimediaIfInstalled n cmd = sometimesExeArgs (n ++ " multimedia control")
  "playerctl" True myMultimediaCtl [cmd]

runTogglePlay :: SometimesX
runTogglePlay = runMultimediaIfInstalled "play/pause" "play-pause"

runPrevTrack :: SometimesX
runPrevTrack = runMultimediaIfInstalled "previous track" "previous"

runNextTrack :: SometimesX
runNextTrack = runMultimediaIfInstalled "next track" "next"

runStopPlay :: SometimesX
runStopPlay = runMultimediaIfInstalled "stop playback" "stop"

--------------------------------------------------------------------------------
-- | Volume Commands

soundDir :: FilePath
soundDir = "sound"

playSound :: MonadIO m => FilePath -> m ()
playSound file = do
  -- manually look up directories to avoid the X monad
  p <- io $ (</> soundDir </> file) . cfgDir <$> getDirectories
  -- paplay seems to have less latency than aplay
  spawnCmd "paplay" [p]

featureSound :: String -> FilePath -> X () -> X () -> SometimesX
featureSound n file pre post =
  sometimesIO_ ("volume " ++ n ++ " control") "paplay" (Only_ $ sysExe "paplay")
  $ pre >> playSound file >> post

runVolumeDown :: SometimesX
runVolumeDown = featureSound "up" volumeChangeSound (return ()) $ void (lowerVolume 2)

runVolumeUp :: SometimesX
runVolumeUp = featureSound "down" volumeChangeSound (return ()) $ void (raiseVolume 2)

runVolumeMute :: SometimesX
runVolumeMute = featureSound "mute" volumeChangeSound (void toggleMute) $ return ()

--------------------------------------------------------------------------------
-- | Notification control

-- TODO test that dunst is actually running (org.freedesktop.Notifications/org.dunstproject.cmd0)
runNotificationCmd :: String -> FilePath -> SometimesX
runNotificationCmd n arg = sometimesIO_ (n ++ " control") "dunstctl" tree cmd
  where
    tree = Only_ $ sysExe myNotificationCtrl
    cmd = spawnCmd myNotificationCtrl [arg]

runNotificationClose :: SometimesX
runNotificationClose = runNotificationCmd "close notification" "close"

runNotificationCloseAll :: SometimesX
runNotificationCloseAll =
  runNotificationCmd "close all notifications" "close-all"

runNotificationHistory :: SometimesX
runNotificationHistory =
  runNotificationCmd "see notification history" "history-pop"

runNotificationContext :: SometimesX
runNotificationContext =
  runNotificationCmd "open notification context" "context"

--------------------------------------------------------------------------------
-- | System commands

-- this is required for some vpn's to work properly with network-manager
-- TODO test that network manager is up
runNetAppDaemon :: Sometimes (IO ProcessHandle)
runNetAppDaemon = sometimesIO_ "network applet" "NM-applet" tree cmd
  where
    tree = Only_ $ localExe "nm-applet"
    cmd = snd <$> spawnPipe "nm-applet"

runToggleBluetooth :: Maybe Client -> SometimesX
runToggleBluetooth cl =
  sometimesDBus cl "bluetooth toggle" "bluetoothctl" tree cmd
  where
    tree = And_ (Only_ $ DBusIO $ sysExe myBluetooth) (Only_ $ Bus btBus)
    cmd _ = spawn
      $ myBluetooth ++ " show | grep -q \"Powered: no\""
      #!&& "a=on"
      #!|| "a=off"
      #!>> fmtCmd myBluetooth ["power", "$a", ">", "/dev/null"]
      #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "bluetooth powered $a" }

runToggleEthernet :: SometimesX
runToggleEthernet = sometimes1 "ethernet toggle" "nmcli" $ IORoot (spawn . cmd) $
  And1 (Only readEthernet) (Only_ $ sysExe "nmcli")
  where
    -- TODO make this less noisy
    cmd iface =
      "nmcli -g GENERAL.STATE device show " ++ iface ++ " | grep -q disconnected"
      #!&& "a=connect"
      #!|| "a=disconnect"
      #!>> fmtCmd "nmcli" ["device", "$a", iface]
      #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "ethernet \"$a\"ed"  }

runStartISyncTimer :: SometimesX
runStartISyncTimer = sometimesIO_ "isync timer" "mbsync timer"
  (Only_ $ sysdUser "mbsync.timer")
  $ spawn
  $ "systemctl --user start mbsync.timer"
  #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "Isync timer started"  }
  #!|| fmtNotifyCmd defNoteError { body = Just $ Text "Isync timer failed to start" }

runStartISyncService :: SometimesX
runStartISyncService = sometimesIO_ "isync" "mbsync service"
  (Only_ $ sysdUser "mbsync.service")
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
  confDir <- asks (cfgDir . directories)
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

-- TODO test that flameshot is actually running (Bus org.flameshot.Flameshot)
runFlameshot :: String -> String -> SometimesX
runFlameshot n mode = sometimesIO_ n myCapture
  (Only_ $ sysExe myCapture) $ spawnCmd myCapture [mode]

-- TODO this will steal focus from the current window (and puts it
-- in the root window?) ...need to fix
runAreaCapture :: SometimesX
runAreaCapture = runFlameshot "screen area capture" "gui"

-- myWindowCap = "screencap -w" --external script

runDesktopCapture :: SometimesX
runDesktopCapture = runFlameshot "fullscreen capture" "full"

runScreenCapture :: SometimesX
runScreenCapture = runFlameshot "screen capture" "screen"

runCaptureBrowser :: SometimesX
runCaptureBrowser = sometimesIO_ "screen capture browser" "feh"
  (Only_ $ sysExe myImageBrowser) $ do
  dir <- io getCaptureDir
  spawnCmd myImageBrowser [dir]
