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
  , runNotificationClose
  , runNotificationCloseAll
  , runNotificationHistory
  , runNotificationContext

  -- daemons
  , runNetAppDaemon

  -- packages
  , networkManagerPkgs
  ) where

import           Control.Monad               (void)
import           Control.Monad.IO.Class

import           Data.Internal.DBus
import           Data.Internal.Dependency

import           DBus

import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Posix.User

import           XMonad                      (asks)
import           XMonad.Actions.Volume
import           XMonad.Core                 hiding (spawn)
import           XMonad.Internal.DBus.Common
import           XMonad.Internal.Notify
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import           XMonad.Operations

--------------------------------------------------------------------------------
-- | My Executables

myTerm :: String
myTerm = "urxvt"

myCalc :: String
myCalc = "bc"

myBrowser :: String
myBrowser = "brave-accel"

myEditor :: String
myEditor = "emacsclient"

myEditorServer :: String
myEditorServer = "emacs"

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
-- | Packages

myTermPkgs :: [Fulfillment]
myTermPkgs = [ Package Official "rxvt-unicode"
             , Package Official "urxvt-perls"
             ]

myEditorPkgs :: [Fulfillment]
myEditorPkgs = [Package Official "emacs-nativecomp"]

notifyPkgs :: [Fulfillment]
notifyPkgs = [Package Official "dunst"]

bluetoothPkgs :: [Fulfillment]
bluetoothPkgs = [Package Official "bluez-utils"]

networkManagerPkgs :: [Fulfillment]
networkManagerPkgs = [Package Official "networkmanager"]

--------------------------------------------------------------------------------
-- | Misc constants

volumeChangeSound :: FilePath
volumeChangeSound = "smb_fireball.wav"

--------------------------------------------------------------------------------
-- | Some nice apps

runTerm :: SometimesX
runTerm = sometimesExe "terminal" "urxvt" myTermPkgs True myTerm

runTMux :: SometimesX
runTMux = sometimesIO_ "terminal multiplexer" "tmux" deps act
  where
    deps = listToAnds (socketExists "tmux" [] socketName)
      $ fmap (sysExe myTermPkgs) [myTerm, "tmux", "bash"]
    act = spawn
      $ "tmux has-session"
      #!&& fmtCmd myTerm ["-e", "bash", "-c", singleQuote c]
      #!|| fmtNotifyCmd defNoteError { body = Just $ Text msg }
    c = "exec tmux attach-session -d"
    msg = "could not connect to tmux session"
    socketName = do
      u <- getEffectiveUserID
      t <- getTemporaryDirectory
      return $ t </> "tmux-" ++ show u </> "default"

runCalc :: SometimesX
runCalc = sometimesIO_ "calculator" "R" deps act
  where
    deps = toAnd_ (sysExe myTermPkgs myTerm) (sysExe [Package Official "bc"] myCalc)
    act = spawnCmd myTerm ["-e", myCalc, "-l"]

runBrowser :: SometimesX
runBrowser = sometimesExe "web browser" "brave" [Package AUR "brave-bin"]
  False myBrowser

runEditor :: SometimesX
runEditor = sometimesIO_ "text editor" "emacs" tree cmd
  where
    cmd = spawnCmd myEditor
      ["-c", "-e", doubleQuote "(select-frame-set-input-focus (selected-frame))"]
    -- NOTE 1: we could test if the emacs socket exists, but it won't come up
    -- before xmonad starts, so just check to see if the process has started
    tree = toAnd_ (sysExe myEditorPkgs myEditor) $ process [] myEditorServer

runFileManager :: SometimesX
runFileManager = sometimesExe "file browser" "pcmanfm" [Package Official "pcmanfm"]
  True "pcmanfm"

--------------------------------------------------------------------------------
-- | Multimedia Commands

runMultimediaIfInstalled :: String -> String -> SometimesX
runMultimediaIfInstalled n cmd = sometimesExeArgs (n ++ " multimedia control")
  "playerctl" [Package Official "playerctl"] True myMultimediaCtl [cmd]

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
  sometimesIO_ ("volume " ++ n ++ " control") "paplay" tree
  $ pre >> playSound file >> post
  where
    tree = Only_ $ sysExe [Package Official "libpulse"] "paplay"

runVolumeDown :: SometimesX
runVolumeDown = featureSound "up" volumeChangeSound (return ()) $ void (lowerVolume 2)

runVolumeUp :: SometimesX
runVolumeUp = featureSound "down" volumeChangeSound (return ()) $ void (raiseVolume 2)

runVolumeMute :: SometimesX
runVolumeMute = featureSound "mute" volumeChangeSound (void toggleMute) $ return ()

--------------------------------------------------------------------------------
-- | Notification control

runNotificationCmd :: String -> FilePath -> Maybe SesClient -> SometimesX
runNotificationCmd n arg cl =
  sometimesDBus cl (n ++ " control") "dunstctl" tree cmd
  where
    cmd _ = spawnCmd myNotificationCtrl [arg]
    tree = toAnd_ (DBusIO $ sysExe notifyPkgs myNotificationCtrl)
      $ Endpoint [] notifyBus notifyPath (interfaceName_ "org.dunstproject.cmd0")
      $ Method_ $ memberName_ "NotificationAction"

runNotificationClose :: Maybe SesClient -> SometimesX
runNotificationClose = runNotificationCmd "close notification" "close"

runNotificationCloseAll :: Maybe SesClient -> SometimesX
runNotificationCloseAll =
  runNotificationCmd "close all notifications" "close-all"

runNotificationHistory :: Maybe SesClient -> SometimesX
runNotificationHistory =
  runNotificationCmd "see notification history" "history-pop"

runNotificationContext :: Maybe SesClient -> SometimesX
runNotificationContext =
  runNotificationCmd "open notification context" "context"

--------------------------------------------------------------------------------
-- | System commands

-- this is required for some vpn's to work properly with network-manager
runNetAppDaemon :: Maybe SysClient -> Sometimes (IO ProcessHandle)
runNetAppDaemon cl = Sometimes "network applet" xpfVPN
  [Subfeature (DBusRoot_ cmd tree cl) "NM-applet"]
  where
    tree = toAnd_ app $ Bus networkManagerPkgs networkManagerBus
    app = DBusIO $ sysExe [Package Official "network-manager-applet"] "nm-applet"
    cmd _ = snd <$> spawnPipe "nm-applet"

runToggleBluetooth :: Maybe SysClient -> SometimesX
runToggleBluetooth cl = Sometimes "bluetooth toggle" xpfBluetooth
  [Subfeature (DBusRoot_ cmd tree cl) "bluetoothctl"]
  where
    tree = And_ (Only_ $ DBusIO $ sysExe bluetoothPkgs myBluetooth) (Only_ $ Bus [] btBus)
    cmd _ = spawn
      $ myBluetooth ++ " show | grep -q \"Powered: no\""
      #!&& "a=on"
      #!|| "a=off"
      #!>> fmtCmd myBluetooth ["power", "$a", ">", "/dev/null"]
      #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "bluetooth powered $a" }

runToggleEthernet :: SometimesX
runToggleEthernet = sometimes1 "ethernet toggle" "nmcli" $ IORoot (spawn . cmd) $
  And1 (Only readEthernet) (Only_ $ sysExe networkManagerPkgs "nmcli")
  where
    -- TODO make this less noisy
    cmd iface =
      "nmcli -g GENERAL.STATE device show " ++ iface ++ " | grep -q disconnected"
      #!&& "a=connect"
      #!|| "a=disconnect"
      #!>> fmtCmd "nmcli" ["device", "$a", iface]
      #!&& fmtNotifyCmd defNoteInfo { body = Just $ Text "ethernet \"$a\"ed"  }

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

runFlameshot :: String -> String -> Maybe SesClient -> SometimesX
runFlameshot n mode cl = sometimesDBus cl n myCapture tree cmd
  where
    cmd _ = spawnCmd myCapture [mode]
    tree = toAnd_ (DBusIO $ sysExe [Package Official "flameshot"] myCapture)
      $ Bus [] $ busName_ "org.flameshot.Flameshot"

-- TODO this will steal focus from the current window (and puts it
-- in the root window?) ...need to fix
runAreaCapture :: Maybe SesClient -> SometimesX
runAreaCapture = runFlameshot "screen area capture" "gui"

-- myWindowCap = "screencap -w" --external script

runDesktopCapture :: Maybe SesClient -> SometimesX
runDesktopCapture = runFlameshot "fullscreen capture" "full"

runScreenCapture :: Maybe SesClient -> SometimesX
runScreenCapture = runFlameshot "screen capture" "screen"

runCaptureBrowser :: SometimesX
runCaptureBrowser = sometimesIO_ "screen capture browser" "feh"
  (Only_ $ sysExe [Package Official "feh"] myImageBrowser) $ do
  dir <- io getCaptureDir
  spawnCmd myImageBrowser [dir]
