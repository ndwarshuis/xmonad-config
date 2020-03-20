{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import ACPI
import DBus.Client (Client)
import Notify
import SendXMsg
import Shell

import DBus.Common
import DBus.IntelBacklight

import qualified Theme as T

import Control.Monad (forM, forM_, mapM_, void, when)

import           Data.List     (find, sortBy, sortOn)
import qualified Data.Map.Lazy as M
import           Data.Maybe    (catMaybes, isJust)
import           Data.Monoid   (All (..))

import Graphics.X11.Types
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrandr

import Control.Arrow     (first)
import Control.Exception

import System.Directory
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import System.Process           (waitForProcess)
import System.Process.Internals
    ( ProcessHandle__ (ClosedHandle, OpenHandle)
    , mkProcessHandle
    , withProcessHandle
    )

import Text.Read (readMaybe)

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Volume
-- import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
-- import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout.BinarySpacePartition (emptyBSP)
-- import XMonad.Layout.DragPane
-- import XMonad.Layout.IM
-- import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
-- import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
-- import XMonad.Prompt.XMonad
-- import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run
-- import XMonad.Util.WindowProperties

import qualified XMonad.StackSet as W

main :: IO ()
main = do
  dbClient <- startXMonadService
  (barPID, h) <- spawnPipe' "xmobar"
  pwrPID <- spawnPID "powermon"
  launch
    $ ewmh
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) (myKeys [pwrPID, barPID] dbClient)
    $ def { terminal = myTerm
          , modMask = myModMask
          , layoutHook = myLayouts
          , manageHook = myManageHook <+> manageDocks <+> manageHook def
          , handleEventHook = myEventHook <+> docksEventHook <+> handleEventHook def
          , startupHook = docksStartupHook <+> startupHook def
          , workspaces = myWorkspaces
          , logHook = myLoghook h
          , clickJustFocuses = False
          , focusFollowsMouse = False
          }

spawnPipe' :: MonadIO m => String -> m (ProcessID, Handle)
spawnPipe' x = io $ do
  (rd, wr) <- createPipe
  setFdOption wr CloseOnExec True
  h <- fdToHandle wr
  hSetBuffering h LineBuffering
  p <- xfork $ do
    _ <- dupTo rd stdInput
    executeFile "/bin/sh" False ["-c", x] Nothing
  closeFd rd
  return (p, h)

myWorkspaces :: [String]
myWorkspaces = map show [1..10 :: Int]

myVMWorkspace :: String
myVMWorkspace = "VM"

myGimpWorkspace :: String
myGimpWorkspace = "GIMP"

myLayouts = onWorkspace myVMWorkspace (noBorders Full)
  -- $ onWorkspace myGimpWorkspace gimpLayout
  $ tall ||| single ||| full
  where
    addTopBar = noFrillsDeco shrinkText T.tabbedTheme
    tall = named "Tall"
      $ avoidStruts
      $ addTopBar
      $ noBorders
      $ Tall 1 0.03 0.5
    single = named "Tabbed"
      -- $ addTopBar
      $ avoidStruts
      $ noBorders
      $ tabbedAlways shrinkText T.tabbedTheme
    full = named "Full"
      $ noBorders Full
    -- gimpLayout = named "Gimp Layout"
    --   $ avoidStruts
    --   $ (tabbedAlways shrinkText defaultTheme) ****||* Full
    --   -- $ withIM (11/64) (Or (Title "Toolbox") (Title "Tool Options"))
    --   -- $ (tabbedAlways shrinkText defaultTheme)

-- | Format workspace and layout in loghook
-- The format will be like "[<1> 2 3] 4 5 | LAYOUT" where each digit
-- is the workspace and LAYOUT is the current layout. Each workspace
-- in the brackets is currently visible and the order reflects the
-- physical location of each screen. The "<>" is the workspace
-- that currently has focus
myLoghook :: Handle -> X ()
myLoghook h = withWindowSet $ io . hPutStrLn h . myWindowSetXinerama

myWindowSetXinerama
  :: LayoutClass layout a1 =>
     W.StackSet String (layout a1) a2 ScreenId ScreenDetail -> String
myWindowSetXinerama ws = wsString ++ sep ++ layout
  where
    wsString = xmobarColor T.backdropFgColor "" $ onscreen ++ offscreen'
    offscreen' = if null offscreen then "" else " " ++ offscreen
    sep = xmobarColor T.backdropFgColor "" " : "
    onscreen = xmobarColor hilightFgColor hilightBgColor
      $ wrap " " " "
      $ unwords
      $ map (fmtTags . W.tag . W.workspace)
      . sortBy compareXCoord
      $ W.current ws : W.visible ws
    fmtTags t = if t == W.currentTag ws
      then xmobarColor T.fgColor hilightBgColor t
      else t
    offscreen = unwords
      $ map W.tag
      . filter (isJust . W.stack)
      . sortOn W.tag
      $ W.hidden ws
    hilightBgColor = "#8fc7ff"
    hilightFgColor = T.blend' 0.5 hilightBgColor T.fgColor
    layout = description . W.layout . W.workspace . W.current $ ws
    compareXCoord s0 s1 = compare x0 x1
      where
        (_, Rectangle x0 _ _ _) = getScreenIdAndRectangle s0
        (_, Rectangle x1 _ _ _) = getScreenIdAndRectangle s1

getFocusedScreen :: X Rectangle
getFocusedScreen = withWindowSet $ return . screenRect . W.screenDetail . W.current

myManageHook :: ManageHook
myManageHook = composeOne
  -- assume virtualbox is not run with the toolbar in fullscreen mode
  -- as this makes a new window that confusingly must go over the
  -- actual VM window
  [ className =? "VirtualBoxVM" -?> doShift myVMWorkspace
  -- the seafile applet
  , className =? "Seafile Client" -?> doFloat
  -- gnucash
  , (className =? "Gnucash" <&&> title =? "Transaction Import Assistant") -?> doFloat
  -- xsane
  , className =? "Xsane" -?> doFloat
  -- all of GIMP
  , className =? "Gimp" -?> doFloat >> doShift myGimpWorkspace
  -- , title =? "GIMP Startup" -?> doIgnore
  -- plots and graphics created by R
  , className =? "R_x11" -?> doFloat
  , className =? "mpv"    -?> doFloat
  -- the floating windows created by the brave browser
  , stringProperty "WM_NAME" =? "Brave" -?> doFloat
  , (stringProperty "WM_WINDOW_ROLE" =? "pop-up"
     <&&> className =? "Brave-browser") -?> doFloat
  -- the dialog windows created by the zotero addon in Google Docs
  , (className =? "Zotero" <&&> resource =? "Toplevel") -?> doFloat
  , isDialog              -?> doCenterFloat
  ]

-- This is a giant hack to "listen" for applications that close. Some
-- apps like Virtualbox go on their own workspace which is dynamically
-- created. But I want said workspace to disappear when the app
-- closes. This is actually hard. We can't just listen to
-- DestroyWindow events as VBox will "destroy" windows when it
-- switches to fullscreen and back. We also can't just monitor the
-- process from the window since WindowDestroy events don't have PIDs
-- attached to them. Therefore, the hack to make this all work is to
-- make a script fire when VirtualBox (and other apps that I want to
-- control in this manner) close. This script fires a bogus
-- ClientMessage event to the root window. This event will have a
-- BITMAP atom (which should do nothing) and a "magic string" in the
-- data field that can be intercepted here. When this event is
-- registered here, close the dynamic workspaces that are empty.
myEventHook :: Event -> X All
myEventHook ClientMessageEvent { ev_message_type = t, ev_data = d }
  | t == bITMAP = do
    let (magic, tag) = splitXMsg d
    if | magic == magicStringWS -> removeEmptyWorkspaceByTag' tag
       | magic == acpiMagic -> do
         let acpiTag = readMaybe tag :: Maybe ACPIEvent
         forM_ acpiTag $ \case
           Power -> myPowerPrompt
           Sleep -> confirmPrompt T.promptTheme "suspend?" runSuspend
           LidClose -> do
             status <- io isDischarging
             forM_ status $ \s -> runScreenLock >> when s runSuspend
       | otherwise -> return ()
    return (All True)
  | otherwise = return (All True)
-- myEventHook DestroyWindowEvent { ev_window = w } = do
--   io $ print w
  -- return (All True)
myEventHook _ = return (All True)

removeEmptyWorkspaceByTag' :: String -> X ()
removeEmptyWorkspaceByTag' tag = do
  -- TODO this function works by first hiding the workspace to be
  -- removed and then removing it. This won't work if there are no
  -- other hidden workspaces to take it's place. So, need to scan
  -- through the list of workspaces and swap the first one that is
  -- empty with the workspace to be removed. If it actually is empty,
  -- this will be enough to make it disappear.
  removeEmptyWorkspaceByTag tag

data PowerPrompt = PowerPrompt

instance XPrompt PowerPrompt where
    showXPrompt PowerPrompt = "(P)oweroff (S)uspend (H)ibernate (R)eboot:"

runScreenLock :: X ()
runScreenLock = spawn "screenlock"

runPowerOff :: X ()
runPowerOff = spawn "systemctl poweroff"

runSuspend :: X ()
runSuspend = spawn "systemctl suspend"

runHibernate :: X ()
runHibernate = spawn "systemctl hibernate"

runReboot :: X ()
runReboot = spawn "systemctl reboot"

myPowerPrompt :: X ()
myPowerPrompt = mkXPrompt PowerPrompt theme comp executeAction
  where
    comp = mkComplFunFromList []
    theme = T.promptTheme { promptKeymap = keymap }
    keymap = M.fromList
      $ ((controlMask, xK_g), quit) :
      map (first $ (,) 0)
      [ (xK_p, sendAction "p")
      , (xK_s, sendAction "s")
      , (xK_h, sendAction "h")
      , (xK_r, sendAction "r")
      , (xK_Return, quit)
      , (xK_Escape, quit)
      ]
    sendAction a = setInput a >> setSuccess True >> setDone True
    executeAction a
      | a == "p" = runPowerOff
      | a == "s" = runScreenLock >> runSuspend
      | a == "h" = runScreenLock >> runHibernate
      | a == "r" = runReboot
      | otherwise = return () -- should never happen

myQuitPrompt :: X ()
myQuitPrompt = confirmPrompt T.promptTheme "quit?" $ io exitSuccess

-- TODO for some reason the screen never wakes up after suspend when
-- the nvidia card is up, so block suspend if nvidia card is running
-- and warn user
isUsingNvidia :: IO Bool
isUsingNvidia = doesDirectoryExist "/sys/module/nvidia"

runOptimusPrompt :: X ()
runOptimusPrompt = do
  nvidiaOn <- io isUsingNvidia
  switch $ if nvidiaOn then "intel" else "nvidia"
  where
    switch mode = confirmPrompt T.promptTheme (prompt mode) (cmd mode)
    prompt mode = "gpu switch to " ++ mode ++ "?"
    cmd mode = spawnCmd "optimus-manager"
      ["--switch", mode, "--no-confirm"]
      >> io exitSuccess

-- shell commands

magicStringWS :: String
magicStringWS = "%%%%%"

spawnCmdOwnWS :: String -> [String] -> String -> X ()
spawnCmdOwnWS cmd args ws = spawn
  $ fmtCmd cmd args
  #!&& fmtCmd "xit-event" [magicStringWS, ws]

myTerm :: String
myTerm = "urxvt"

runTerm :: X ()
runTerm = spawn myTerm

runCalc :: X ()
runCalc = spawnCmd myTerm ["-e", "R"]

myDmenuCmd :: String
myDmenuCmd = "rofi"

-- | Focus rofi on the current workspace always
-- Apparently xrandr and xinerama order monitors differently, which
-- means they have different indices. Since rofi uses the former and
-- xmonad uses the latter, this function is to figure out the xrandr
-- screen name based on the xinerama screen that is currently in
-- focus. The steps to do this:
-- 1) get the coordinates of the currently focuses xinerama screen
-- 2) get list of Xrandr outputs and filter which ones are connected
-- 3) match the coordinates of the xinerama screen with the xrandr
--    output and return the latter's name (eg "DP-0") which can be
--    fed to Rofi
getMonitorName :: X (Maybe String)
getMonitorName = do
  dpy <- asks display
  root <- asks theRoot
  -- these are the focused screen coordinates according to xinerama
  (sx, sy) <- getCoords
  io $ do
    res <- xrrGetScreenResourcesCurrent dpy root
    outputs <- forM res $ \res' ->
      forM (xrr_sr_outputs res') $ \output -> do
        oi <- xrrGetOutputInfo dpy res' output
        case oi of
          -- connection: 0 == connected, 1 == disconnected
          Just XRROutputInfo { xrr_oi_connection = 0
                             , xrr_oi_name = name
                             , xrr_oi_crtc = crtc
                             } -> do
            ci <- xrrGetCrtcInfo dpy res' crtc
            return $ (\ci' -> Just (name, xrr_ci_x ci', xrr_ci_y ci'))
              =<< ci
          _ -> return Nothing
    return $ (\(name, _, _) -> Just name)
      =<< find (\(_, x, y) -> x == sx && y == sy) . catMaybes
      =<< outputs
  where
    getCoords = do
      (Rectangle x y _ _) <- getFocusedScreen
      return (fromIntegral x, fromIntegral y)

spawnDmenuCmd :: String -> [String] -> X ()
spawnDmenuCmd cmd args = do
  name <- getMonitorName
  case name of
    Just n  -> spawnCmd cmd $ ["-m", n] ++ args
    Nothing -> io $ putStrLn "fail"

spawnDmenuCmd' :: [String] -> X ()
spawnDmenuCmd' = spawnDmenuCmd myDmenuCmd

runCmdMenu :: X ()
runCmdMenu = spawnDmenuCmd' ["-show", "run"]

runAppMenu :: X ()
runAppMenu = spawnDmenuCmd' ["-show", "drun"]

runClipMenu :: X ()
runClipMenu = spawnDmenuCmd'
  [ "-modi", "\"clipboard:greenclip print\""
  , "-show", "clipboard"
  , "-run-command", "'{cmd}'"
  , "-theme-str", "'#element.selected.normal { background-color: #00c44e; }'"
  ]

runWinMenu :: X ()
runWinMenu = spawnDmenuCmd' ["-show", "window"]

runNetMenu :: X ()
runNetMenu = spawnDmenuCmd "networkmanager_dmenu" []

runDevMenu :: X ()
runDevMenu = spawnDmenuCmd "rofi-devices" []

runBrowser :: X ()
runBrowser = spawn "brave"

runEditor :: X ()
runEditor = spawnCmd "emacsclient"
  ["-c", "-e", "\"(select-frame-set-input-focus (selected-frame))\""]

runFileManager :: X ()
runFileManager = spawn "pcmanfm"

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

runVBox :: X ()
runVBox = spawnCmdOwnWS "vbox-start win8raw" [] myVMWorkspace

runGimp :: X ()
runGimp = spawnCmdOwnWS "gimp" [] myGimpWorkspace

runCleanup :: [ProcessID] -> Client -> X ()
runCleanup ps client = io $ do
  mapM_ killPID ps
  stopXMonadService client

killPID :: ProcessID -> IO ()
killPID pID = do
  h <- mkProcessHandle pID False
  -- this may fail of the PID does not exist
  _ <- try $ sendSIGTERM h :: IO (Either IOException ())
  -- this may fail if the process exits instantly and the handle
  -- is destroyed by the time we get to this line (I think?)
  _ <- try $ waitForProcess h :: IO (Either IOException ExitCode)
  return ()
  where
    sendSIGTERM h = withProcessHandle h $ \case
      OpenHandle _ -> signalProcess sigTERM pID
      ClosedHandle _ -> return ()
      _ -> return () -- this should never happen

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

showWorkspace :: WorkspaceId -> X ()
showWorkspace tag = windows $ W.view tag

enableDPMS :: X ()
enableDPMS = spawnCmd "xset" ["s", "on", "+dpms"]

disableDPMS :: X ()
disableDPMS = spawnCmd "xset" ["s", "off", "-dpms"]

-- keybindings

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ do
  name <- getMonitorName
  case name of
    Just n -> do
      h <- spawnPipe $ cmd n
      io $ hPutStr h (unlines $ showKm x)
      io $ hClose h
      return ()
    Nothing -> io $ putStrLn "fail"
  where cmd name = fmtCmd myDmenuCmd
          [ "-dmenu"
          , "-m"
          , name
          , "-p"
          , "commands"
          , "-theme-str"
          , "'#element.selected.normal { background-color: #a200ff; }'"
          ]

myModMask :: KeyMask
myModMask = mod4Mask

mkNamedSubmap
  :: XConfig l
     -> String
     -> [(String, NamedAction)]
     -> [((KeyMask, KeySym), NamedAction)]
mkNamedSubmap c sectionName bindings =
  (subtitle sectionName:) $ mkNamedKeymap c bindings

-- NOTE: the following bindings are used by dunst:
-- "M-~", "M-<esc>", "M-S-<esc>", "M-S-."
myKeys :: [ProcessID] -> Client -> XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys hs client c =
  mkNamedSubmap c "Window Layouts"
  [ ("M-j", addName "focus down" $ windows W.focusDown)
  , ("M-k", addName "focus up" $ windows W.focusUp)
  , ("M-m", addName "focus master" $ windows W.focusMaster)
  , ("M-S-j", addName "swap down" $ windows W.swapDown)
  , ("M-S-k", addName "swap up" $ windows W.swapUp)
  , ("M-S-m", addName "swap master" $ windows W.swapMaster)
  -- TODO this will decrement past 0?
  , ("M-C-j", addName "remove master window" $ sendMessage (IncMasterN (-1)))
  , ("M-C-k", addName "add master window" $ sendMessage (IncMasterN 1))
  , ("M-<Return>", addName "next layout" $ sendMessage NextLayout)
  , ("M-S-<Return>", addName "reset layout" $ setLayout $ XMonad.layoutHook c)
  , ("M-t", addName "sink tiling" $ withFocused $ windows . W.sink)
  , ("M--", addName "shrink" $ sendMessage Shrink)
  , ("M-=", addName "expand" $ sendMessage Expand)
  ] ++

  mkNamedSubmap c "Workspaces"
  -- NOTE this assumes that there are workspaces bound to numbers
  ([ (mods ++ show i, addName (msg ++ " " ++ show i) $ windows $ f w)
    | (w, i) <- zip (XMonad.workspaces c) [1..] :: [(String, Int)]
    , (mods, msg, f) <-
      [ ("M-", "switch to workspace", W.view)
      , ("M-S-", "move client to workspace", W.shift)]
  ] ++
  [ ("M-v", addName "switch to VM workspace" $ showWorkspace myVMWorkspace)
  , ("M-M1-g", addName "switch to Gimp workspace" $ showWorkspace myGimpWorkspace)
  ]) ++

  mkNamedSubmap c "Screens"
  [ ("M-l", addName "move up screen" nextScreen)
  , ("M-h", addName "move down screen" prevScreen)
  , ("M-C-l", addName "move client up screen" $ shiftNextScreen >> nextScreen)
  , ("M-C-h", addName "move client down screen" $ shiftPrevScreen >> prevScreen)
  , ("M-S-l", addName "shift up screen" $ swapNextScreen >> nextScreen)
  , ("M-S-h", addName "shift down screen" $ swapPrevScreen >> prevScreen)
  ] ++

  mkNamedSubmap c "Actions"
  [ ("M-q", addName "close window" kill1)
  , ("M-r", addName "run program" runCmdMenu)
  , ("M-C-s", addName "capture area" runAreaCapture)
  , ("M-C-S-s", addName "capture screen" runScreenCapture)
  , ("M-C-d", addName "capture desktop" runDesktopCapture)
  -- , ("M-C-S-s", addName "capture focused window" $ spawn myWindowCap)
  , ("M-<Delete>", addName "lock screen" runScreenLock)
  ] ++

  mkNamedSubmap c "Launchers"
  [ ("<XF86Search>", addName "select/launch app" runAppMenu)
  , ("M-g", addName "launch clipboard manager" runClipMenu)
  , ("M-a", addName "launch network selector" runNetMenu)
  , ("M-w", addName "launch window selector" runWinMenu)
  , ("M-u", addName "launch device selector" runDevMenu)
  , ("M-C-e", addName "launch editor" runEditor)
  , ("M-C-w", addName "launch browser" runBrowser)
  , ("M-C-t", addName "launch terminal" runTerm)
  , ("M-C-q", addName "launch calc" runCalc)
  , ("M-C-f", addName "launch file manager" runFileManager)
  , ("M-C-v", addName "launch windows VM" $ runVBox >> appendWorkspace myVMWorkspace)
  , ("M-C-g", addName "launch GIMP" $ runGimp >> appendWorkspace myGimpWorkspace)
  ] ++

  mkNamedSubmap c "Multimedia"
  [ ("<XF86AudioPlay>", addName "toggle play/pause" runTogglePlay)
  , ("<XF86AudioPrev>", addName "previous track" runPrevTrack)
  , ("<XF86AudioNext>", addName "next track" runNextTrack)
  , ("<XF86AudioStop>", addName "stop" runStopPlay)
  , ("<XF86AudioLowerVolume>", addName "volume down" runVolumeDown)
  , ("<XF86AudioRaiseVolume>", addName "volume up" runVolumeUp)
  , ("<XF86AudioMute>", addName "volume mute" runVolumeMute)
  , ("M-C-b", addName "toggle bluetooth" runToggleBluetooth)
  ] ++

  mkNamedSubmap c "System"
  [ ("M-.", addName "backlight up" runIncBacklight)
  , ("M-,", addName "backlight down" runDecBacklight)
  , ("M-M1-,", addName "backlight min" runMinBacklight)
  , ("M-M1-.", addName "backlight max" runMaxBacklight)
  , ("M-M1-=", addName "enable screensaver" enableDPMS)
  , ("M-M1--", addName "disable screensaver" disableDPMS)
  , ("M-<F2>", addName "restart xmonad" $ runCleanup hs client >> runRestart)
  , ("M-S-<F2>", addName "recompile xmonad" runRecompile)
  , ("M-<End>", addName "power menu" myPowerPrompt)
  , ("M-<Home>", addName "quit xmonad" myQuitPrompt)
  , ("M-<Esc>", addName "switch gpu" runOptimusPrompt)
  ]
