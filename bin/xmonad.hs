{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           ACPI
import           DBus.Common
import           DBus.IntelBacklight
import           DBus.Screensaver
import           Notify
import           Process
import           SendXMsg
import           Shell
import qualified Theme                            as T
import           WorkspaceMon

import           Control.Arrow                    (first)
import           Control.Concurrent
import           Control.Monad
    ( forM
    , forM_
    , liftM2
    , mapM_
    , void
    , when
    )

import           Data.List
    ( find
    , isPrefixOf
    , sortBy
    , sortOn
    )
import qualified Data.Map.Lazy                    as M
import           Data.Maybe                       (catMaybes, isJust)
import           Data.Monoid                      (All (..))

import           DBus.Client                      (Client)

import           Graphics.X11.Types
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xrandr

import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix.IO
import           System.Posix.Process
import           System.Posix.Types

import           Text.Read                        (readMaybe)

import           Xmobar.Common

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.Volume
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
-- import           XMonad.Layout.LayoutCombinators  hiding ((|||))
-- import           XMonad.Layout.Master
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
-- import           XMonad.Layout.Simplest
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.Run

main :: IO ()
main = do
  dbClient <- startXMonadService
  (barPID, h) <- spawnPipe' "xmobar"
  _ <- forkIO runPowermon
  _ <- forkIO runWorkspaceMon'
  launch
    $ ewmh
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) (mkKeys [barPID] dbClient)
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
          , normalBorderColor = T.bordersColor
          , focusedBorderColor = T.selectedBordersColor
          }

runWorkspaceMon' :: IO ()
runWorkspaceMon' = runWorkspaceMon
  $ fromList [ (myGimpClass, myGimpWorkspace)
             , (myVMClass, myVMWorkspace)
             , (myXSaneClass, myXSaneWorkspace)
             ]

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

myLayouts = onWorkspace myVMWorkspace vmLayout
  $ onWorkspace myGimpWorkspace gimpLayout
  $ tall ||| single ||| full
  where
    addTopBar = noFrillsDeco shrinkText T.tabbedTheme
    tall = renamed [Replace "Tall"]
      $ avoidStruts
      $ addTopBar
      $ noBorders
      $ Tall 1 0.03 0.5
    single = renamed [Replace "Tabbed"]
      $ avoidStruts
      $ noBorders
      $ tabbedAlways shrinkText T.tabbedTheme
    full = renamed [Replace "Full"]
      $ noBorders Full
    vmLayout = noBorders Full
    -- TODO use a tabbed layout for multiple master windows
    gimpLayout = renamed [Replace "Gimp Layout"]
      $ avoidStruts
      $ noBorders
      $ addTopBar
      $ Tall 1 0.025 0.8

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
    wsString = wrapColorBg T.backdropFgColor "" $ onscreen ++ offscreen'
    offscreen' = if null offscreen then "" else " " ++ offscreen
    sep = wrapColorBg T.backdropFgColor "" " : "
    onscreen = wrapColorBg hilightFgColor hilightBgColor
      $ (\s -> " " ++ s ++ " ")
      $ unwords
      $ map (fmtTags . W.tag . W.workspace)
      . sortBy compareXCoord
      $ W.current ws : W.visible ws
    fmtTags t = if t == W.currentTag ws
      then wrapColorBg T.fgColor hilightBgColor t
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

viewShift = doF . liftM2 (.) W.view W.shift

appendViewShift tag = liftX (appendWorkspace tag) >> viewShift tag

($?) :: Query a -> (a -> Bool) -> Query Bool
($?) q f = f <$> q

matchGimpRole :: String -> Query Bool
matchGimpRole role = stringProperty "WM_WINDOW_ROLE" $? isPrefixOf role
  <&&> className =? myGimpClass

moveBottom :: W.StackSet i l a s sd -> W.StackSet i l a s sd
moveBottom = W.modify' $ \(W.Stack f t b) -> W.Stack f (reverse b ++ t) []

myManageHook :: ManageHook
myManageHook = composeOne
  [ isDialog -?> doCenterFloat
  -- VM window
  , className =? myVMClass -?> appendViewShift myVMWorkspace
  -- GIMP
  , matchGimpRole "gimp-image-window" -?> appendViewShift myGimpWorkspace
  , matchGimpRole "gimp-dock" -?> doF (moveBottom . W.focusMaster)
  , matchGimpRole "gimp-toolbox" -?> doF (moveBottom . W.focusMaster)
  , className =? myGimpClass -?> appendViewShift myGimpWorkspace
  -- XSane
  , className =? myXSaneClass -?> appendViewShift myXSaneWorkspace >> doFloat
  -- the seafile applet
  , className =? "Seafile Client" -?> doFloat
  -- gnucash
  , (className =? "Gnucash" <&&> title =? "Transaction Import Assistant") -?> doFloat
  -- plots and graphics created by R
  , className =? "R_x11" -?> doFloat
  , className =? "mpv"    -?> doFloat
  -- the floating windows created by the brave browser
  , stringProperty "WM_NAME" =? "Brave" -?> doFloat
  , (stringProperty "WM_WINDOW_ROLE" =? "pop-up"
     <&&> className =? "Brave-browser") -?> doFloat
  -- the dialog windows created by the zotero addon in Google Docs
  , (className =? "Zotero" <&&> resource =? "Toplevel") -?> doFloat
  ]

myEventHook :: Event -> X All
myEventHook ClientMessageEvent { ev_message_type = t, ev_data = d }
  | t == bITMAP = do
    let (xtype, tag) = splitXMsg d
    case xtype of
      Workspace -> removeEmptyWorkspaceByTag tag
      ACPI -> do
         let acpiTag = toEnum <$> readMaybe tag :: Maybe ACPIEvent
         forM_ acpiTag $ \case
           Power -> runPowerPrompt
           Sleep -> confirmPrompt T.promptTheme "suspend?" runSuspend
           LidClose -> do
             status <- io isDischarging
             forM_ status $ \s -> runScreenLock >> when s runSuspend
    return (All True)
myEventHook _ = return (All True)

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

data PowerAction = Poweroff
    | Shutdown
    | Hibernate
    | Reboot
    deriving (Eq)

instance Enum PowerAction where
  toEnum 0 = Poweroff
  toEnum 1 = Shutdown
  toEnum 2 = Hibernate
  toEnum 3 = Reboot
  toEnum _ = errorWithoutStackTrace "Main.Enum.PowerAction.toEnum: bad argument"

  fromEnum Poweroff  = 0
  fromEnum Shutdown  = 1
  fromEnum Hibernate = 2
  fromEnum Reboot    = 3

runPowerPrompt :: X ()
runPowerPrompt = mkXPrompt PowerPrompt theme comp executeAction
  where
    comp = mkComplFunFromList []
    theme = T.promptTheme { promptKeymap = keymap }
    keymap = M.fromList
      $ ((controlMask, xK_g), quit) :
      map (first $ (,) 0)
      [ (xK_p, sendAction Poweroff)
      , (xK_s, sendAction Shutdown)
      , (xK_h, sendAction Hibernate)
      , (xK_r, sendAction Reboot)
      , (xK_Return, quit)
      , (xK_Escape, quit)
      ]
    sendAction a = setInput (show $ fromEnum a) >> setSuccess True >> setDone True
    executeAction a = case toEnum $ read a of
      Poweroff  -> runPowerOff
      Shutdown  -> runScreenLock >> runSuspend
      Hibernate -> runScreenLock >> runHibernate
      Reboot    -> runReboot

runQuitPrompt :: X ()
runQuitPrompt = confirmPrompt T.promptTheme "quit?" $ io exitSuccess

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

getFocusedScreen :: X Rectangle
getFocusedScreen = withWindowSet $ return . screenRect . W.screenDetail . W.current

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

runCleanup :: [ProcessID] -> Client -> X ()
runCleanup ps client = io $ do
  mapM_ killPID ps
  stopXMonadService client

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

runToggleDPMS :: X ()
runToggleDPMS = io $ void callToggle

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

myVMWorkspace :: String
myVMWorkspace = "VM"

myVMClass :: String
myVMClass = "VirtualBoxVM"

myGimpWorkspace :: String
myGimpWorkspace = "GIMP"

-- TODO I don't feel like changing the version long term
myGimpClass :: String
myGimpClass = "Gimp-2.10"

myXSaneWorkspace :: String
myXSaneWorkspace = "XSANE"

myXSaneClass :: String
myXSaneClass = "Xsane"

wsOccupied :: Eq a1 => a1 -> W.StackSet a1 l a2 sid sd -> Bool
wsOccupied tag ws = elem tag $ map W.tag $ filter (isJust .  W.stack)
  -- list of all workspaces with windows on them
  -- TODO is there not a better way to do this?
  $ W.workspace (W.current ws) : W.hidden ws ++ map W.workspace (W.visible ws)

spawnOrSwitch :: WorkspaceId -> X () -> X ()
spawnOrSwitch tag cmd = do
  occupied <- withWindowSet $ return . wsOccupied tag
  if occupied then windows $ W.view tag else cmd

runVBox :: X ()
runVBox = spawnOrSwitch myVMWorkspace $ spawnCmd "vbox-start" ["win8raw"]

runGimp :: X ()
runGimp = spawnOrSwitch myGimpWorkspace $ spawnCmd "gimp-2.10" []

runXSane :: X ()
runXSane = spawnOrSwitch myXSaneWorkspace $ spawnCmd "xsane" []

myModMask :: KeyMask
myModMask = mod4Mask

mkNamedSubmap
  :: XConfig l
     -> String
     -> [(String, String, X ())]
     -> [((KeyMask, KeySym), NamedAction)]
mkNamedSubmap c sectionName bindings =
  (subtitle sectionName:) $ mkNamedKeymap c
  $ map (\(key, name, cmd) -> (key, addName name cmd)) bindings

mkKeys :: [ProcessID] -> Client -> XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
mkKeys hs client c =
  mkNamedSubmap c "Window Layouts"
  [ ("M-j", "focus down", windows W.focusDown)
  , ("M-k", "focus up", windows W.focusUp)
  , ("M-m", "focus master", windows W.focusMaster)
  , ("M-S-j", "swap down", windows W.swapDown)
  , ("M-S-k", "swap up", windows W.swapUp)
  , ("M-S-m", "swap master", windows W.swapMaster)
  , ("M-<Return>", "next layout", sendMessage NextLayout)
  , ("M-S-<Return>", "reset layout", setLayout $ layoutHook c)
  , ("M-t", "sink tiling", withFocused $ windows . W.sink)
  , ("M--", "shrink", sendMessage Shrink)
  , ("M-=", "expand", sendMessage Expand)
  , ("M-S--", "remove master window", sendMessage $ IncMasterN (-1))
  , ("M-S-=", "add master window", sendMessage $ IncMasterN 1)
  ] ++

  mkNamedSubmap c "Workspaces"
  -- ASSUME standard workspaces only use numbers 0-9 (otherwise we won't get
  -- valid keysyms)
  ([ (mods ++ n, msg ++ n, f n) | n <- myWorkspaces
    , (mods, msg, f) <-
      [ ("M-", "switch to workspace ", windows . W.view)
      , ("M-S-", "move client to workspace ", windows . W.shift)
      , ("M-C-", "follow client to workspace ", \n' -> do
            windows $ W.shift n'
            windows $ W.view n')
      ]
   ] ++
   [ ("M-M1-l", "move up workspace", moveTo Next HiddenNonEmptyWS)
   , ("M-M1-h", "move down workspace", moveTo Prev HiddenNonEmptyWS)
   ]) ++

  mkNamedSubmap c "Screens"
  [ ("M-l", "move up screen", nextScreen)
  , ("M-h", "move down screen", prevScreen)
  , ("M-C-l", "follow client up screen", shiftNextScreen >> nextScreen)
  , ("M-C-h", "follow client down screen", shiftPrevScreen >> prevScreen)
  , ("M-S-l", "shift workspace up screen", swapNextScreen >> nextScreen)
  , ("M-S-h", "shift workspace down screen", swapPrevScreen >> prevScreen)
  ] ++

  mkNamedSubmap c "Actions"
  [ ("M-q", "close window", kill1)
  , ("M-r", "run program", runCmdMenu)
  , ("M-C-s", "capture area", runAreaCapture)
  , ("M-C-S-s", "capture screen", runScreenCapture)
  , ("M-C-d", "capture desktop", runDesktopCapture)
  -- , ("M-C-S-s", "capture focused window", spawn myWindowCap)
  ] ++

  mkNamedSubmap c "Launchers"
  [ ("<XF86Search>", "select/launch app", runAppMenu)
  , ("M-g", "launch clipboard manager", runClipMenu)
  , ("M-a", "launch network selector", runNetMenu)
  , ("M-w", "launch window selector", runWinMenu)
  , ("M-u", "launch device selector", runDevMenu)
  , ("M-C-e", "launch editor", runEditor)
  , ("M-C-w", "launch browser", runBrowser)
  , ("M-C-t", "launch terminal", runTerm)
  , ("M-C-q", "launch calc", runCalc)
  , ("M-C-f", "launch file manager", runFileManager)
  , ("M-C-v", "launch windows VM", runVBox)
  , ("M-C-g", "launch GIMP", runGimp)
  , ("M-C-x", "launch XSane", runXSane)
  ] ++

  mkNamedSubmap c "Multimedia"
  [ ("<XF86AudioPlay>", "toggle play/pause", runTogglePlay)
  , ("<XF86AudioPrev>", "previous track", runPrevTrack)
  , ("<XF86AudioNext>", "next track", runNextTrack)
  , ("<XF86AudioStop>", "stop", runStopPlay)
  , ("<XF86AudioLowerVolume>", "volume down", runVolumeDown)
  , ("<XF86AudioRaiseVolume>", "volume up", runVolumeUp)
  , ("<XF86AudioMute>", "volume mute", runVolumeMute)
  ] ++

  -- dummy map for dunst commands (defined separately but this makes them show
  -- up in the help menu)
  mkNamedSubmap c "Dunst"
  [ ("M-`", "dunst history", return ())
  , ("M-S-`", "dunst close", return ())
  , ("M-M1-`", "dunst context menu", return ())
  , ("M-C-`", "dunst close all", return ())
  ] ++

  mkNamedSubmap c "System"
  [ ("M-.", "backlight up", runIncBacklight)
  , ("M-,", "backlight down", runDecBacklight)
  , ("M-M1-,", "backlight min", runMinBacklight)
  , ("M-M1-.", "backlight max", runMaxBacklight)
  , ("M-<End>", "power menu", runPowerPrompt)
  , ("M-<Home>", "quit xmonad", runQuitPrompt)
  , ("M-<Delete>", "lock screen", runScreenLock)
  -- M-<F1> reserved for showing the keymap
  , ("M-<F2>", "restart xmonad", runCleanup hs client >> runRestart)
  , ("M-<F3>", "recompile xmonad", runRecompile)
  , ("M-<F10>", "toggle bluetooth", runToggleBluetooth)
  , ("M-<F11>", "toggle screensaver", runToggleDPMS)
  , ("M-<F12>", "switch gpu", runOptimusPrompt)
  ]
