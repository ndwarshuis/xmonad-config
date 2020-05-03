{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}

--------------------------------------------------------------------------------
-- | XMonad binary

module Main (main) where

import           Control.Concurrent

import           Data.List
    ( isPrefixOf
    , sortBy
    , sortOn
    )
import           Data.Maybe                                   (isJust)
import           Data.Monoid                                  (All (..))

import           Graphics.X11.Types
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Extras

import           System.IO
import           System.Process

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.Warp
import           XMonad.Hooks.DynamicLog
    ( pad
    , wrap
    , xmobarColor
    )
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Internal.Command.Desktop
import           XMonad.Internal.Command.DMenu
import           XMonad.Internal.Command.Power
import           XMonad.Internal.Concurrent.ACPIEvent
import           XMonad.Internal.Concurrent.ClientMessage
import           XMonad.Internal.Concurrent.DynamicWorkspaces
import           XMonad.Internal.DBus.Control
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import qualified XMonad.Internal.Theme                        as T
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet                              as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions

main :: IO ()
main = do
  cl <- startXMonadService
  (h, p) <- spawnPipe "xmobar"
  _ <- forkIO runPowermon
  _ <- forkIO $ runWorkspaceMon allDWs
  let ts = ThreadState
        { client = cl
        , childPIDs = [p]
        , childHandles = [h]
        }
  launch
    $ ewmh
    $ addKeymap ts
    $ def { terminal = myTerm
          , modMask = myModMask
          , layoutHook = myLayouts
          , manageHook = myManageHook
          , handleEventHook = myEventHook
          , startupHook = myStartupHook
          , workspaces = myWorkspaces
          , logHook = myLoghook h
          , clickJustFocuses = False
          , focusFollowsMouse = False
          , normalBorderColor = T.bordersColor
          , focusedBorderColor = T.selectedBordersColor
          }

--------------------------------------------------------------------------------
-- | Concurrency configuration

data ThreadState = ThreadState
    { client       :: Client
    , childPIDs    :: [ProcessHandle]
    , childHandles :: [Handle]
    }

-- TODO shouldn't this be run by a signal handler?
runCleanup :: ThreadState -> X ()
runCleanup ts = io $ do
  mapM_ killHandle $ childPIDs ts
  stopXMonadService $ client ts

--------------------------------------------------------------------------------
-- | Startuphook configuration

myStartupHook :: X ()
myStartupHook = setDefaultCursor xC_left_ptr <+> docksStartupHook
  <+> startupHook def

--------------------------------------------------------------------------------
-- | Workspace configuration

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..10 :: Int]

gimpDynamicWorkspace :: DynWorkspace
gimpDynamicWorkspace = DynWorkspace
  { dwName = "Gimp"
  , dwTag = t
  , dwClass = c
  , dwHook =
    [ matchGimpRole "gimp-image-window" -?> appendViewShift t
    , matchGimpRole "gimp-dock" -?> doF (toBottom . W.focusMaster)
    , matchGimpRole "gimp-toolbox" -?> doF (toBottom . W.focusMaster)
    , className =? c -?> appendViewShift t
    ]
  , dwKey = 'g'
  , dwCmd = Just $ spawnCmd "gimp-2.10" []
  }
  where
    matchGimpRole role = isPrefixOf role <$> stringProperty "WM_WINDOW_ROLE"
      <&&> className =? c
    toBottom = W.modify' $ \(W.Stack f tp bt) -> W.Stack f (reverse bt ++ tp) []
    t = "GIMP"
    c = "Gimp-2.10" -- TODO I don't feel like changing the version long term

wmDynamicWorkspace :: DynWorkspace
wmDynamicWorkspace = DynWorkspace
  { dwName = "Windows VirtualBox"
  , dwTag = t
  , dwClass = c
  , dwHook = [ className =? c -?> appendViewShift t ]
  , dwKey = 'v'
  , dwCmd = Just $ spawnCmd "vbox-start" ["win8raw"]
  }
  where
    t = "VM"
    c = "VirtualBoxVM"

xsaneDynamicWorkspace :: DynWorkspace
xsaneDynamicWorkspace = DynWorkspace
  { dwName = "XSane"
  , dwTag = t
  , dwClass = c
  , dwHook = [ className =? c -?> appendViewShift t >> doFloat ]
  , dwKey = 'x'
  , dwCmd = Just $ spawnCmd "xsane" []
  }
  where
    t = "XSANE"
    c = "Xsane"

steamDynamicWorkspace :: DynWorkspace
steamDynamicWorkspace = DynWorkspace
  { dwName = "Steam Game"
  , dwTag = t
  , dwClass = c
  -- TODO not sure why the doSink is needed, windows should tile be default
  -- since dynamic workspaces are first in the managehook
  , dwHook = [ className =? c -?> appendViewShift t >> doSink ]
  , dwKey = 'z'
  , dwCmd = Nothing
  }
  where
    t = "STEAM"
    -- TODO this actually needs to be a list to match all games we care about
    c = "portal2_linux"

allDWs :: [DynWorkspace]
allDWs = [ xsaneDynamicWorkspace
         , wmDynamicWorkspace
         , gimpDynamicWorkspace
         , steamDynamicWorkspace
         ]

--------------------------------------------------------------------------------
-- | Layout configuration

myLayouts = onWorkspace (dwTag wmDynamicWorkspace) vmLayout
  $ onWorkspace (dwTag gimpDynamicWorkspace) gimpLayout
  $ onWorkspace (dwTag steamDynamicWorkspace) steamLayout
  $ mkToggle (single HIDE)
  $ tall ||| fulltab ||| full
  where
    addTopBar = noFrillsDeco shrinkText T.tabbedTheme
    tall = renamed [Replace "Tall"]
      $ avoidStruts
      $ addTopBar
      $ noBorders
      $ Tall 1 0.03 0.5
    fulltab = renamed [Replace "Tabbed"]
      $ avoidStruts
      $ noBorders
      $ tabbedAlways shrinkText T.tabbedTheme
    full = renamed [Replace "Full"]
      $ noBorders Full
    vmLayout = noBorders Full
    steamLayout = vmLayout
    -- TODO use a tabbed layout for multiple master windows
    gimpLayout = renamed [Replace "Gimp Layout"]
      $ avoidStruts
      $ noBorders
      $ addTopBar
      $ Tall 1 0.025 0.8

-- | Make a new empty layout and add a message to show/hide it. This is useful
-- for quickly showing conky.
data EmptyLayout a = EmptyLayout
    deriving (Show, Read)

instance LayoutClass EmptyLayout a where
  doLayout a b _ = emptyLayout a b
  description _ = "Desktop"

data HIDE = HIDE
    deriving (Read, Show, Eq, Typeable)

instance Transformer HIDE Window where
  transform _ x k = k EmptyLayout (\EmptyLayout -> x)

-- TODO toggle back to normal when a new window is opened
runHide :: X ()
runHide = sendMessage $ Toggle HIDE

--------------------------------------------------------------------------------
-- | Loghook configuration
--
-- The format will be like "[<1> 2 3] 4 5 | LAYOUT (N)" where each digit is the
-- workspace and LAYOUT is the current layout. Each workspace in the brackets is
-- currently visible and the order reflects the physical location of each
-- screen. The "<>" is the workspace that currently has focus. N is the number
-- of windows on the current workspace.

myLoghook :: Handle -> X ()
myLoghook h = withWindowSet $ io . hPutStrLn h . myWindowSetXinerama

myWindowSetXinerama
  :: LayoutClass layout a1 =>
     W.StackSet String (layout a1) a2 ScreenId ScreenDetail -> String
myWindowSetXinerama ws =
  unwords $ filter (not . null) [onScreen, offScreen, sep, layout, nWindows]
  where
    onScreen = xmobarColor hilightFgColor hilightBgColor
      $ pad
      $ unwords
      $ map (fmtTags . W.tag . W.workspace)
      $ sortBy compareXCoord
      $ W.current ws : W.visible ws
    offScreen = xmobarColor T.backdropFgColor ""
      $ unwords
      $ map W.tag
      $ filter (isJust . W.stack)
      $ sortOn W.tag
      $ W.hidden ws
    sep = xmobarColor T.backdropFgColor "" ":"
    layout = description $ W.layout $ W.workspace $ W.current ws
    nWindows = wrap "(" ")"
      $ show
      $ length
      $ W.integrate'
      $ W.stack
      $ W.workspace
      $ W.current ws
    hilightBgColor = "#8fc7ff"
    hilightFgColor = T.blend' 0.5 hilightBgColor T.fgColor
    fmtTags t = if t == W.currentTag ws
      then xmobarColor T.fgColor hilightBgColor t
      else t

compareXCoord
  :: W.Screen i1 l1 a1 ScreenId ScreenDetail
     -> W.Screen i2 l2 a2 ScreenId ScreenDetail -> Ordering
compareXCoord s0 s1 = compare x0 x1
  where
    (_, Rectangle x0 _ _ _) = getScreenIdAndRectangle s0
    (_, Rectangle x1 _ _ _) = getScreenIdAndRectangle s1

--------------------------------------------------------------------------------
-- | Managehook configuration

myManageHook :: ManageHook
myManageHook = manageApps <+> manageDocks <+> manageHook def

manageApps :: ManageHook
manageApps = composeOne $ concatMap dwHook allDWs ++
  [ isDialog -?> doCenterFloat
  -- the seafile applet
  , className =? "Seafile Client" -?> doFloat
  -- gnucash
  , (className =? "Gnucash" <&&> title =? "Transaction Import Assistant") -?> doFloat
  -- plots and graphics
  , className =? "R_x11" -?> doFloat
  , className =? "Matplotlib" -?> doFloat
  , className =? "mpv"    -?> doFloat
  -- the floating windows created by the brave browser
  , stringProperty "WM_NAME" =? "Brave" -?> doFloat
  -- , (stringProperty "WM_WINDOW_ROLE" =? "pop-up"
  --    <&&> className =? "Brave-browser") -?> doFloat
  -- the dialog windows created by the zotero addon in Google Docs
  , (className =? "Zotero" <&&> resource =? "Toplevel") -?> doFloat
  ]

--------------------------------------------------------------------------------
-- | Eventhook configuration

myEventHook :: Event -> X All
myEventHook = xMsgEventHook <+> docksEventHook <+> handleEventHook def

-- | React to ClientMessage events from concurrent threads
xMsgEventHook :: Event -> X All
xMsgEventHook ClientMessageEvent { ev_message_type = t, ev_data = d }
  | t == bITMAP = do
    let (xtype, tag) = splitXMsg d
    case xtype of
      Workspace -> removeDynamicWorkspace tag
      ACPI      -> handleACPI tag
    return (All True)
xMsgEventHook _ = return (All True)

--------------------------------------------------------------------------------
-- | Keymap configuration

myModMask :: KeyMask
myModMask = mod4Mask

addKeymap :: ThreadState -> XConfig l -> XConfig l
addKeymap ts = addDescrKeys' ((myModMask, xK_F1), runShowKeys) (mkKeys ts)

mkKeys :: ThreadState -> XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
mkKeys ts c =
  mkNamedSubmap "Window Layouts"
  [ ("M-j", "focus down", windows W.focusDown)
  , ("M-k", "focus up", windows W.focusUp)
  , ("M-m", "focus master", windows W.focusMaster)
  , ("M-d", "focus master", runHide)
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

  mkNamedSubmap "Workspaces"
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

  mkNamedSubmap "Dynamic Workspaces"
  [ ("M-C-" ++ [k], "launch/switch to " ++ n, cmd)
  |  DynWorkspace { dwTag = t, dwKey = k, dwCmd = a, dwName = n } <- allDWs,
     let cmd = case a of
           Just a' -> spawnOrSwitch t a'
           Nothing -> windows $ W.view t
  ] ++

  mkNamedSubmap "Screens"
  [ ("M-l", "move up screen", nextScreen)
  , ("M-h", "move down screen", prevScreen)
  , ("M-C-l", "follow client up screen", shiftNextScreen >> nextScreen)
  , ("M-C-h", "follow client down screen", shiftPrevScreen >> prevScreen)
  , ("M-S-l", "shift workspace up screen", swapNextScreen >> nextScreen)
  , ("M-S-h", "shift workspace down screen", swapPrevScreen >> prevScreen)
  ] ++

  mkNamedSubmap "Actions"
  [ ("M-q", "close window", kill1)
  , ("M-r", "run program", runCmdMenu)
  , ("M-<Space>", "warp pointer", warpToWindow 0.5 0.5)
  , ("M-C-s", "capture area", runAreaCapture)
  , ("M-C-S-s", "capture screen", runScreenCapture)
  , ("M-C-d", "capture desktop", runDesktopCapture)
  , ("M-C-b", "browse captures", runCaptureBrowser)
  -- , ("M-C-S-s", "capture focused window", spawn myWindowCap)
  ] ++

  mkNamedSubmap "Launchers"
  [ ("<XF86Search>", "select/launch app", runAppMenu)
  , ("M-g", "launch clipboard manager", runClipMenu)
  , ("M-a", "launch network selector", runNetMenu)
  , ("M-w", "launch window selector", runWinMenu)
  , ("M-u", "launch device selector", runDevMenu)
  , ("M-b", "launch bitwarden selector", runBwMenu)
  , ("M-C-e", "launch editor", runEditor)
  , ("M-C-w", "launch browser", runBrowser)
  , ("M-C-t", "launch terminal", runTerm)
  , ("M-C-q", "launch calc", runCalc)
  , ("M-C-f", "launch file manager", runFileManager)
  ] ++

  mkNamedSubmap "Multimedia"
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
  mkNamedSubmap "Dunst"
  [ ("M-`", "dunst history", return ())
  , ("M-S-`", "dunst close", return ())
  , ("M-M1-`", "dunst context menu", return ())
  , ("M-C-`", "dunst close all", return ())
  ] ++

  mkNamedSubmap "System"
  [ ("M-.", "backlight up", runIncBacklight)
  , ("M-,", "backlight down", runDecBacklight)
  , ("M-M1-,", "backlight min", runMinBacklight)
  , ("M-M1-.", "backlight max", runMaxBacklight)
  , ("M-<End>", "power menu", runPowerPrompt)
  , ("M-<Home>", "quit xmonad", runQuitPrompt)
  , ("M-<Delete>", "lock screen", runScreenLock)
  -- M-<F1> reserved for showing the keymap
  , ("M-<F2>", "restart xmonad", runCleanup ts >> runRestart)
  , ("M-<F3>", "recompile xmonad", runRecompile)
  , ("M-<F10>", "toggle bluetooth", runToggleBluetooth)
  , ("M-<F11>", "toggle screensaver", runToggleDPMS)
  , ("M-<F12>", "switch gpu", runOptimusPrompt)
  ]
  where
    mkNamedSubmap header bindings = (subtitle header:) $ mkNamedKeymap c
      $ map (\(key, name, cmd) -> (key, addName name cmd)) bindings
