{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- | XMonad binary

module Main (main) where

import           Control.Concurrent
import           Control.Monad                                (unless)

import           Data.List
    ( isPrefixOf
    , sortBy
    , sortOn
    )
import           Data.Maybe                                   (isJust, mapMaybe)
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
import           XMonad.Internal.Command.DMenu
import           XMonad.Internal.Command.Desktop
import           XMonad.Internal.Command.Power
import           XMonad.Internal.Concurrent.ACPIEvent
import           XMonad.Internal.Concurrent.ClientMessage
import           XMonad.Internal.Concurrent.DynamicWorkspaces
import           XMonad.Internal.Concurrent.Removable
import           XMonad.Internal.DBus.Control
import           XMonad.Internal.DBus.IntelBacklight
import           XMonad.Internal.DBus.Screensaver
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import qualified XMonad.Internal.Theme                        as T
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed
import qualified XMonad.Operations                            as O
import qualified XMonad.StackSet                              as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState                  as E
import           XMonad.Util.NamedActions
import           XMonad.Util.WorkspaceCompare

main :: IO ()
main = do
  (cl, bc, sc) <- startXMonadService
  (h, p) <- spawnPipe "xmobar"
  _ <- forkIO runPowermon
  _ <- forkIO runRemovableMon
  _ <- forkIO $ runWorkspaceMon allDWs
  let ts = ThreadState
        { client = cl
        , childPIDs = [p]
        , childHandles = [h]
        }
  (ekbs, missing) <- fmap filterExternal $ evalExternal $ externalBindings bc sc ts
  mapM_ warnMissing missing
  -- IDK why this is necessary; nothing prior to this line will print if missing
  hFlush stdout
  launch
    $ ewmh
    $ addKeymap ekbs
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

-- TODO add _NET_DESKTOP_VIEWPORTS to _NET_SUPPORTED?
myStartupHook :: X ()
myStartupHook = setDefaultCursor xC_left_ptr
  <+> docksStartupHook
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
    , matchGimpRole "gimp-dock" -?> doF W.swapDown
    , matchGimpRole "gimp-toolbox" -?> doF W.swapDown
    , className =? c -?> appendViewShift t
    ]
  , dwKey = 'g'
  , dwCmd = Just $ spawnCmd "gimp-2.10" []
  }
  where
    matchGimpRole role = isPrefixOf role <$> stringProperty "WM_WINDOW_ROLE"
      <&&> className =? c
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

allDWs :: [DynWorkspace]
allDWs = [ xsaneDynamicWorkspace
         , wmDynamicWorkspace
         , gimpDynamicWorkspace
         ]

--------------------------------------------------------------------------------
-- | Layout configuration

myLayouts = onWorkspace (dwTag wmDynamicWorkspace) vmLayout
  $ onWorkspace (dwTag gimpDynamicWorkspace) gimpLayout
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

myLoghook :: Handle -> X ()
myLoghook h = do
  logXinerama h
  logViewports

-- | Viewports loghook
-- This is all stuff that should probably be added to the EVMH contrib module.
-- Basically, this will send the workspace "viewport" positions to
-- _NET_DESKTOP_VIEWPORT which can be further processed by tools such as
-- 'wmctrl' to figure out which workspaces are on what monitor outside of
-- xmomad. This is more or less the way i3 does this, where the current
-- workspace has a valid position and everything else is just (0, 0). Also, I
-- probably should set the _NET_SUPPORT atom to reflect the existance of
-- _NET_DESKTOP_VIEWPORT, but for now there seems to be no ill effects so why
-- bother...(if that were necessary it would go in the startup hook)
newtype DesktopViewports = DesktopViewports [Int]
    deriving Eq

instance ExtensionClass DesktopViewports where
    initialValue = DesktopViewports []

logViewports :: X ()
logViewports = withWindowSet $ \s -> do
  sort' <- getSortByIndex
  let ws = sort' $ W.workspaces s
  let desktopViewports = concatMap (wsToViewports s) ws
  whenChanged (DesktopViewports desktopViewports) $
      setDesktopViewports desktopViewports
  where
    wsToViewports s w = let cur = W.current s in
      if W.tag w == currentTag cur then currentPos cur else [0, 0]
    currentTag = W.tag . W.workspace
    currentPos = rectXY . screenRect . W.screenDetail
    rectXY (Rectangle x y _ _) = [fromIntegral x, fromIntegral y]

setDesktopViewports :: [Int] -> X ()
setDesktopViewports vps = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_DESKTOP_VIEWPORT"
    c <- getAtom "CARDINAL"
    io $ changeProperty32 dpy r a c propModeReplace $ map fromIntegral vps

-- stolen from XMonad.Hooks.EwmhDesktops
whenChanged :: (Eq a, ExtensionClass a) => a -> X () -> X ()
whenChanged v action = do
    v0 <- E.get
    unless (v == v0) $ do
        action
        E.put v

-- | Xinerama loghook (for xmobar)
-- The format will be like "[<1> 2 3] 4 5 | LAYOUT (N)" where each digit is the
-- workspace and LAYOUT is the current layout. Each workspace in the brackets is
-- currently visible and the order reflects the physical location of each
-- screen. The "<>" is the workspace that currently has focus. N is the number
-- of windows on the current workspace.

logXinerama :: Handle -> X ()
logXinerama h = withWindowSet $ \ws -> io
  $ hPutStrLn h
  $ unwords
  $ filter (not . null) [onScreen ws, offScreen ws, sep, layout ws, nWindows ws]
  where
    onScreen ws = xmobarColor hilightFgColor hilightBgColor
      $ pad
      $ unwords
      $ map (fmtTags ws . W.tag . W.workspace)
      $ sortBy compareXCoord
      $ W.current ws : W.visible ws
    offScreen ws = xmobarColor T.backdropFgColor ""
      $ unwords
      $ map W.tag
      $ filter (isJust . W.stack)
      $ sortOn W.tag
      $ W.hidden ws
    sep = xmobarColor T.backdropFgColor "" ":"
    layout ws = description $ W.layout $ W.workspace $ W.current ws
    nWindows ws = wrap "(" ")"
      $ show
      $ length
      $ W.integrate'
      $ W.stack
      $ W.workspace
      $ W.current ws
    hilightBgColor = "#8fc7ff"
    hilightFgColor = T.blend' 0.5 hilightBgColor T.fgColor
    fmtTags ws t = if t == W.currentTag ws
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

addKeymap :: [KeyGroup (X ())] -> XConfig l -> XConfig l
addKeymap external = addDescrKeys' ((myModMask, xK_F1), runShowKeys)
  (\c -> concatMap (mkNamedSubmap c) $ internalBindings c ++ external)

internalBindings :: XConfig Layout -> [KeyGroup (X ())]
internalBindings c =
  [ KeyGroup "Window Layouts"
    [ KeyBinding "M-j" "focus down" $ windows W.focusDown
    , KeyBinding "M-k" "focus up" $ windows W.focusUp
    , KeyBinding "M-m" "focus master" $ windows W.focusMaster
    , KeyBinding "M-d" "focus master" runHide
    , KeyBinding "M-S-j" "swap down" $ windows W.swapDown
    , KeyBinding "M-S-k" "swap up" $ windows W.swapUp
    , KeyBinding "M-S-m" "swap master" $ windows W.swapMaster
    , KeyBinding "M-<Return>" "next layout" $ sendMessage NextLayout
    , KeyBinding "M-S-<Return>" "reset layout" $ setLayout $ layoutHook c
    , KeyBinding "M-t" "sink tiling" $ withFocused $ windows . W.sink
    , KeyBinding "M-S-t" "float tiling" $ withFocused O.float
    , KeyBinding "M--" "shrink" $ sendMessage Shrink
    , KeyBinding "M-=" "expand" $ sendMessage Expand
    , KeyBinding "M-S--" "remove master window" $ sendMessage $ IncMasterN (-1)
    , KeyBinding "M-S-=" "add master window" $ sendMessage $ IncMasterN 1
    ]

  , KeyGroup "Workspaces"
    -- ASSUME standard workspaces only use numbers 0-9 (otherwise we won't get
    -- valid keysyms)
    ([ KeyBinding (mods ++ n) (msg ++ n) (f n) | n <- myWorkspaces
      , (mods, msg, f) <-
        [ ("M-", "switch to workspace ", windows . W.view)
        , ("M-S-", "move client to workspace ", windows . W.shift)
        , ("M-C-", "follow client to workspace ", \n' -> do
              windows $ W.shift n'
              windows $ W.view n')
        ]
     ] ++
     [ KeyBinding "M-M1-l" "move up workspace" $ moveTo Next HiddenNonEmptyWS
     , KeyBinding "M-M1-h" "move down workspace" $ moveTo Prev HiddenNonEmptyWS
     ])

  , KeyGroup "Dynamic Workspaces"
    [ KeyBinding ("M-C-" ++ [k]) ("launch/switch to " ++ n) cmd
    |  DynWorkspace { dwTag = t, dwKey = k, dwCmd = a, dwName = n } <- allDWs,
       let cmd = case a of
             Just a' -> spawnOrSwitch t a'
             Nothing -> windows $ W.view t
    ]

  , KeyGroup "Screens"
    [ KeyBinding "M-l" "move up screen" nextScreen
    , KeyBinding "M-h" "move down screen" prevScreen
    , KeyBinding "M-C-l" "follow client up screen" $ shiftNextScreen >> nextScreen
    , KeyBinding "M-C-h" "follow client down screen" $ shiftPrevScreen >> prevScreen
    , KeyBinding "M-S-l" "shift workspace up screen" $ swapNextScreen >> nextScreen
    , KeyBinding "M-S-h" "shift workspace down screen" $ swapPrevScreen >> prevScreen
    ]

  -- dummy map for dunst commands (defined separately but this makes them show
  -- up in the help menu)
  , KeyGroup "Dunst"
    [ KeyBinding "M-`" "dunst history" skip
    , KeyBinding "M-S-`" "dunst close" skip
    , KeyBinding "M-M1-`" "dunst context menu" skip
    , KeyBinding "M-C-`" "dunst close all" skip
    ]
  ]

mkNamedSubmap :: XConfig Layout ->  KeyGroup (X ()) -> [((KeyMask, KeySym), NamedAction)]
mkNamedSubmap c KeyGroup { kgHeader = h, kgBindings = b } =
  (subtitle h:) $ mkNamedKeymap c
  $ (\KeyBinding{kbSyms = s, kbDesc = d, kbAction = a} -> (s, addName d a))
  <$> b

data KeyBinding a = KeyBinding
  { kbSyms   :: String
  , kbDesc   :: String
  , kbAction :: a
  }

data KeyGroup a = KeyGroup
  { kgHeader   :: String
  , kgBindings :: [KeyBinding a]
  }

evalExternal :: [KeyGroup (IO MaybeX)] -> IO [KeyGroup MaybeX]
evalExternal = mapM go
  where
    go k@KeyGroup { kgBindings = bs } =
      (\bs' -> k { kgBindings = bs' }) <$> mapM evalKeyBinding bs

evalKeyBinding :: Monad m => KeyBinding (m a) -> m (KeyBinding a)
evalKeyBinding k@KeyBinding { kbAction = a } = (\b -> k { kbAction = b }) <$> a

filterExternal :: [KeyGroup MaybeX] -> ([KeyGroup (X ())], [Dependency])
filterExternal kgs = let kgs' = fmap go kgs in (fst <$> kgs', concatMap snd kgs')
  where
    go k@KeyGroup { kgBindings = bs } = let bs' = go' <$> bs in
      (k { kgBindings = mapMaybe fst bs' }, concatMap snd bs')
    go' k@KeyBinding{ kbDesc = d, kbAction = a } = case a of
      Installed x ds -> (Just $ k{ kbAction = x }, ds)
      Missing ds     -> (Just $ k{ kbDesc = flagMissing d, kbAction = skip }, ds)
      Ignore         -> (Nothing, [])
    flagMissing s = "[!!!]" ++ s

externalBindings :: Maybe BacklightControls
  -> MaybeExe SSControls
  -> ThreadState
  -> [KeyGroup (IO MaybeX)]
externalBindings bc sc ts =
  [ KeyGroup "Launchers"
    [ KeyBinding "<XF86Search>" "select/launch app" runAppMenu
    , KeyBinding "M-g" "launch clipboard manager" runClipMenu
    , KeyBinding "M-a" "launch network selector" runNetMenu
    , KeyBinding "M-w" "launch window selector" runWinMenu
    , KeyBinding "M-u" "launch device selector" runDevMenu
    , KeyBinding "M-b" "launch bitwarden selector" runBwMenu
    , KeyBinding "M-C-e" "launch editor" runEditor
    , KeyBinding "M-C-w" "launch browser" runBrowser
    , KeyBinding "M-C-t" "launch terminal with tmux" runTMux
    , KeyBinding "M-C-S-t" "launch terminal" runTerm
    , KeyBinding "M-C-q" "launch calc" runCalc
    , KeyBinding "M-C-f" "launch file manager" runFileManager
    ]

  , KeyGroup "Actions"
    [ KeyBinding "M-q" "close window" $ noCheck kill1
    , KeyBinding "M-r" "run program" runCmdMenu
    , KeyBinding "M-<Space>" "warp pointer" $ noCheck $ warpToWindow 0.5 0.5
    , KeyBinding "M-C-s" "capture area" runAreaCapture
    , KeyBinding "M-C-S-s" "capture screen" runScreenCapture
    , KeyBinding "M-C-d" "capture desktop" runDesktopCapture
    , KeyBinding "M-C-b" "browse captures" runCaptureBrowser
    -- , ("M-C-S-s", "capture focused window", spawn myWindowCap)
    ]

  , KeyGroup "Multimedia"
    [ KeyBinding "<XF86AudioPlay>" "toggle play/pause" runTogglePlay
    , KeyBinding "<XF86AudioPrev>" "previous track" runPrevTrack
    , KeyBinding "<XF86AudioNext>" "next track" runNextTrack
    , KeyBinding "<XF86AudioStop>" "stop" runStopPlay
    , KeyBinding "<XF86AudioLowerVolume>" "volume down" runVolumeDown
    , KeyBinding "<XF86AudioRaiseVolume>" "volume up" runVolumeUp
    , KeyBinding "<XF86AudioMute>" "volume mute" runVolumeMute
    ]

  , KeyGroup "System"
    [ KeyBinding "M-." "backlight up" $ runMaybe bc backlightUp
    , KeyBinding "M-," "backlight down" $ runMaybe bc backlightDown
    , KeyBinding "M-M1-," "backlight min" $ runMaybe bc backlightMin
    , KeyBinding "M-M1-." "backlight max" $ runMaybe bc backlightMax
    , KeyBinding "M-<End>" "power menu" $ noCheck runPowerPrompt
    , KeyBinding "M-<Home>" "quit xmonad" $ noCheck runQuitPrompt
    , KeyBinding "M-<Delete>" "lock screen" runScreenLock
    -- M-<F1> reserved for showing the keymap
    , KeyBinding "M-<F2>" "restart xmonad" $ noCheck (runCleanup ts >> runRestart)
    , KeyBinding "M-<F3>" "recompile xmonad" $ noCheck runRecompile
    , KeyBinding "M-<F7>" "start Isync Service" runStartISyncService
    , KeyBinding "M-C-<F7>" "start Isync Timer" runStartISyncTimer
    , KeyBinding "M-<F8>" "select autorandr profile" runAutorandrMenu
    , KeyBinding "M-<F9>" "toggle ethernet" runToggleEthernet
    , KeyBinding "M-<F10>" "toggle bluetooth" runToggleBluetooth
    , KeyBinding "M-<F11>" "toggle screensaver" $ return $ fmap (io . ssToggle) sc
    , KeyBinding "M-<F12>" "switch gpu" runOptimusPrompt
    ]
  ]
  where
    -- TODO this is hacky, I shouldn't really need this data structure for
    -- something that doesn't depend on executables
    runMaybe c f = return $ maybe Ignore (\x -> Installed (io $ f x) []) c

