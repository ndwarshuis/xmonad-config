{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- | XMonad binary

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.Lifted                      (fork)
import           Control.Monad

import           Data.List
    ( intercalate
    , isPrefixOf
    , sortBy
    , sortOn
    )
import           Data.Maybe
import           Data.Monoid                                    (All (..))

import           Graphics.X11.Types
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Extras

import           System.Environment
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
import           XMonad.Internal.Concurrent.VirtualBox
import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.Common
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Control
import           XMonad.Internal.DBus.Removable
import           XMonad.Internal.DBus.Screensaver
import           XMonad.Internal.Dependency
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import qualified XMonad.Internal.Theme                          as T
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed
import qualified XMonad.Operations                              as O
import qualified XMonad.StackSet                                as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState                    as E
import           XMonad.Util.NamedActions
import           XMonad.Util.WorkspaceCompare

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse []         = run
parse ["--deps"] = withCache printDeps
parse _          = usage

run :: IO ()
run = do
  db <- connectDBusX
  conf <- withCache $ evalConf db
  ds <- getDirectories
  -- IDK why this is necessary; nothing prior to this will print if missing
  hFlush stdout
  launch conf ds

data FeatureSet = FeatureSet
  { fsKeys          :: ThreadState -> DBusState -> [KeyGroup FeatureX]
  , fsDBusExporters :: [Maybe Client -> SometimesIO]
  , fsPowerMon      :: SometimesIO
  , fsRemovableMon  :: Maybe Client -> SometimesIO
  , fsDaemons       :: [Sometimes (IO ProcessHandle)]
  , fsACPIHandler   :: Always (String -> X ())
  , fsTabbedTheme   :: Always Theme
  , fsDynWorkspaces :: [Sometimes DynWorkspace]
  , fsShowKeys      :: Always ([((KeyMask, KeySym), NamedAction)] -> X ())
  }

features :: FeatureSet
features = FeatureSet
  { fsKeys = externalBindings
  , fsDBusExporters = dbusExporters
  , fsPowerMon = runPowermon
  , fsRemovableMon = runRemovableMon
  , fsACPIHandler = runHandleACPI
  , fsDynWorkspaces = allDWs'
  , fsTabbedTheme = T.tabbedFeature
  , fsShowKeys = runShowKeys
  , fsDaemons = [ runNetAppDaemon
                , runFlameshotDaemon
                  -- TODO the problem with launching
                  -- dunst here is that the history
                  -- will get nuked on each restart
                , runNotificationDaemon
                , runBwDaemon
                  -- TODO does this have a lag when
                  -- spawned within the WM?
                , runClipManager
                , runAutolock
                ]
  }

evalConf db = do
  -- start DBus interfaces first since many features after this test these
  -- interfaces as dependencies
  startDBusInterfaces
  (xmobarHandle, ts) <- startChildDaemons
  startRemovableMon
  startPowerMon
  dws <- startDynWorkspaces
  tt <- evalAlways $ fsTabbedTheme features
  -- fb <- evalAlways $ fsFontBuilder features
  kbs <- filterExternal <$> evalExternal (fsKeys features ts db)
  sk <- evalAlways $ fsShowKeys features
  ha <- evalAlways $ fsACPIHandler features
  return $ ewmh
    $ addKeymap dws sk kbs
    $ docks
    $ def { terminal = myTerm
          , modMask = myModMask
          , layoutHook = myLayouts tt
          , manageHook = myManageHook dws
          , handleEventHook = myEventHook ha
          , startupHook = myStartupHook
          , workspaces = myWorkspaces
          , logHook = myLoghook xmobarHandle
          , clickJustFocuses = False
          , focusFollowsMouse = False
          , normalBorderColor = T.bordersColor
          , focusedBorderColor = T.selectedBordersColor
          }
  where
    forkIO_ = void . forkIO
    startDBusInterfaces = mapM_ (\f -> executeSometimes $ f $ dbSesClient db)
      $ fsDBusExporters features
    startChildDaemons = do
      (h, p) <- io $ spawnPipe "xmobar"
      ps <- catMaybes <$> mapM executeSometimes (fsDaemons features)
      return (h, ThreadState (p:ps) [h])
    startRemovableMon = void $ executeSometimes $ fsRemovableMon features
                        $ dbSysClient db
    startPowerMon = void $ fork $ void $ executeSometimes $ fsPowerMon features
    startDynWorkspaces = do
      dws <- catMaybes <$> mapM evalSometimes (fsDynWorkspaces features)
      io $ forkIO_ $ runWorkspaceMon dws
      return dws

printDeps :: FIO ()
printDeps = do
  db <- io connectDBus
  (i, f, d) <- allFeatures db
  is <- mapM dumpSometimes i
  fs <- mapM dumpFeature f
  ds <- mapM dumpSometimes d
  let (UQ u) = jsonArray $ fmap JSON_UQ $ is ++ fs ++ ds
  io $ putStrLn u
  io $ disconnectDBus db

allFeatures :: DBusState -> FIO ([SometimesIO], [FeatureX], [Sometimes DynWorkspace])
allFeatures db = do
  let bfs = concatMap (fmap kbMaybeAction . kgBindings)
            $ externalBindings ts db
  let dbus = fmap (\f -> f $ dbSesClient db) dbusExporters
  let others = [runRemovableMon $ dbSysClient db, runPowermon]
  return (dbus ++ others, Left runScreenLock:bfs, allDWs')
  where
    ts = ThreadState { tsChildPIDs = [], tsChildHandles = [] }

usage :: IO ()
usage = putStrLn $ intercalate "\n"
  [ "xmonad: run greatest window manager"
  , "xmonad --deps: print dependencies"
  ]

--------------------------------------------------------------------------------
-- | Concurrency configuration

data ThreadState = ThreadState
    { tsChildPIDs    :: [ProcessHandle]
    , tsChildHandles :: [Handle]
    }

-- TODO shouldn't this be run by a signal handler?
runCleanup :: ThreadState -> DBusState -> X ()
runCleanup ts db = io $ do
  mapM_ killHandle $ tsChildPIDs ts
  disconnectDBusX db

--------------------------------------------------------------------------------
-- | Startuphook configuration

-- TODO add _NET_DESKTOP_VIEWPORTS to _NET_SUPPORTED?
myStartupHook :: X ()
myStartupHook = setDefaultCursor xC_left_ptr
  <+> startupHook def

--------------------------------------------------------------------------------
-- | Workspace configuration

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..10 :: Int]

gimpTag :: String
gimpTag = "GIMP"

vmTag :: String
vmTag = "VM"

xsaneTag :: String
xsaneTag = "XSANE"

f5Tag :: String
f5Tag = "F5VPN"

gimpDynamicWorkspace :: Sometimes DynWorkspace
gimpDynamicWorkspace = sometimesIO_ "gimp workspace" "gimp" tree dw
  where
    tree = Only_ $ sysExe "gimp"
    dw = DynWorkspace
         { dwName = "Gimp"
         , dwTag = gimpTag
         , dwClass = c
         , dwHook =
           [ matchGimpRole "gimp-image-window" -?> appendViewShift gimpTag
           , matchGimpRole "gimp-dock" -?> doF W.swapDown
           , matchGimpRole "gimp-toolbox" -?> doF W.swapDown
           , className =? c -?> appendViewShift gimpTag
           ]
         , dwKey = 'g'
         , dwCmd = Just $ spawnCmd "gimp-2.10" []
         }
    matchGimpRole role = isPrefixOf role <$> stringProperty "WM_WINDOW_ROLE"
      <&&> className =? c
    c = "Gimp-2.10" -- TODO I don't feel like changing the version long term

vmDynamicWorkspace :: Sometimes DynWorkspace
vmDynamicWorkspace = sometimes1 "virtualbox workspace" "windows 8 VM" root
  where
    root = IORoot_ dw $ And_ (Only_ $ sysExe "VBoxManage")
      $ Only_ $ IOTest_ name $ vmExists vm
    name = unwords ["test if", vm, "exists"]
    c = "VirtualBoxVM"
    vm = "win8raw"
    dw = DynWorkspace
         { dwName = "Windows VirtualBox"
         , dwTag = vmTag
         , dwClass = c
         , dwHook = [ className =? c -?> appendViewShift vmTag ]
         , dwKey = 'v'
         , dwCmd = Just $ spawnCmd "vbox-start" [vm]
         }

-- -- TODO this shell command is hilariously slow and kills my fast startup time
-- vmExists :: String -> IO (Maybe String)
-- vmExists vm =
--   go <$> tryIOError (readCreateProcessWithExitCode' pr "")
--   where
--     pr = proc' "VBoxManage" ["showvminfo", vm]
--     go (Right (ExitSuccess, _, _))   = Nothing
--     go (Right (ExitFailure _, _, _)) = Just $ "VM not found: " ++ vm
--     go (Left e)                      = Just $ show e

xsaneDynamicWorkspace :: Sometimes DynWorkspace
xsaneDynamicWorkspace = sometimesIO_ "scanner workspace" "xsane" tree dw
  where
    tree = Only_ $ sysExe "xsane"
    dw = DynWorkspace
         { dwName = "XSane"
         , dwTag = xsaneTag
         , dwClass = c
         , dwHook = [ className =? c -?> appendViewShift xsaneTag >> doFloat ]
         , dwKey = 'x'
         , dwCmd = Just $ spawnCmd "xsane" []
         }
    c = "Xsane"

f5vpnDynamicWorkspace :: Sometimes DynWorkspace
f5vpnDynamicWorkspace = sometimesIO_ "F5 VPN workspace" "f5vpn" tree dw
  where
    tree = Only_ $ sysExe "f5vpn"
    dw = DynWorkspace
         { dwName = "F5Vpn"
         , dwTag = f5Tag
         , dwClass = c
         , dwHook = [ className =? c -?> appendShift f5Tag ]
         , dwKey = 'i'
         , dwCmd = Just skip
         }
    c = "F5 VPN"

allDWs' :: [Sometimes DynWorkspace]
allDWs' = [xsaneDynamicWorkspace
          , vmDynamicWorkspace
          , gimpDynamicWorkspace
          , f5vpnDynamicWorkspace
          ]

--------------------------------------------------------------------------------
-- | Layout configuration

-- NOTE this will have all available layouts, even those that may be for
-- features that failed. Trying to dynamically take out a layout seems to
-- make a new type :/
myLayouts tt = onWorkspace vmTag vmLayout
  $ onWorkspace gimpTag gimpLayout
  $ mkToggle (single HIDE)
  $ tall ||| fulltab ||| full
  where
    addTopBar = noFrillsDeco shrinkText tt
    tall = renamed [Replace "Tall"]
      $ avoidStruts
      $ addTopBar
      $ noBorders
      $ Tall 1 0.03 0.5
    fulltab = renamed [Replace "Tabbed"]
      $ avoidStruts
      $ noBorders
      $ tabbedAlways shrinkText tt
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
    hilightBgColor = "#A6D3FF"
    hilightFgColor = T.blend' 0.4 hilightBgColor T.fgColor
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

myManageHook :: [DynWorkspace] -> ManageHook
myManageHook dws = manageApps dws <+> manageHook def

manageApps :: [DynWorkspace] -> ManageHook
manageApps dws = composeOne $ concatMap dwHook dws ++
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

myEventHook :: (String -> X ()) -> Event -> X All
myEventHook handler = xMsgEventHook handler <+> handleEventHook def

-- | React to ClientMessage events from concurrent threads
xMsgEventHook :: (String -> X ()) -> Event -> X All
xMsgEventHook handler ClientMessageEvent { ev_message_type = t, ev_data = d }
  | t == bITMAP = do
    let (xtype, tag) = splitXMsg d
    case xtype of
      Workspace -> removeDynamicWorkspace tag
      ACPI      -> handler tag
      Unknown   -> io $ print "WARNING: unknown concurrent message"
    return (All True)
xMsgEventHook _ _ = return (All True)

--------------------------------------------------------------------------------
-- | Keymap configuration

myModMask :: KeyMask
myModMask = mod4Mask

addKeymap :: [DynWorkspace] -> ([((KeyMask, KeySym), NamedAction)] -> X ())
  -> [KeyGroup (X ())] -> XConfig l -> XConfig l
addKeymap dws showKeys external = addDescrKeys' ((myModMask, xK_F1), showKeys)
  (\c -> concatMap (mkNamedSubmap c) $ internalBindings dws c ++ external)

internalBindings :: [DynWorkspace] -> XConfig Layout -> [KeyGroup (X ())]
internalBindings dws c =
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
     [ KeyBinding "M-M1-l" "move up workspace" $ moveTo Next (hiddenWS :&: Not emptyWS)
     , KeyBinding "M-M1-h" "move down workspace" $ moveTo Prev (hiddenWS :&: Not emptyWS)
     ])

  , KeyGroup "Dynamic Workspaces"
    [ KeyBinding ("M-C-" ++ [k]) ("launch/switch to " ++ n) cmd
    |  DynWorkspace { dwTag = t, dwKey = k, dwCmd = a, dwName = n } <- dws,
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
  ]

mkNamedSubmap :: XConfig Layout ->  KeyGroup (X ()) -> [((KeyMask, KeySym), NamedAction)]
mkNamedSubmap c KeyGroup { kgHeader = h, kgBindings = b } =
  (subtitle h:) $ mkNamedKeymap c
  $ (\KeyBinding{kbSyms = s, kbDesc = d, kbMaybeAction = a} -> (s, addName d a))
  <$> b

data KeyBinding a = KeyBinding
  { kbSyms        :: String
  , kbDesc        :: String
  , kbMaybeAction :: a
  }

data KeyGroup a = KeyGroup
  { kgHeader   :: String
  , kgBindings :: [KeyBinding a]
  }

evalExternal :: [KeyGroup FeatureX] -> FIO [KeyGroup MaybeX]
evalExternal = mapM go
  where
    go k@KeyGroup { kgBindings = bs } =
      (\bs' -> k { kgBindings = bs' }) <$> mapM evalKeyBinding bs

evalKeyBinding :: KeyBinding FeatureX -> FIO (KeyBinding MaybeX)
evalKeyBinding k@KeyBinding { kbMaybeAction = a } =
  (\f -> k { kbMaybeAction = f }) <$> evalFeature a

filterExternal :: [KeyGroup MaybeX] -> [KeyGroup (X ())]
filterExternal = fmap go
  where
    go k@KeyGroup { kgBindings = bs } = k { kgBindings = mapMaybe flagKeyBinding bs }

flagKeyBinding :: KeyBinding MaybeX -> Maybe (KeyBinding (X ()))
flagKeyBinding k@KeyBinding{ kbDesc = d, kbMaybeAction = a } = case a of
  (Just x) -> Just $ k{ kbMaybeAction = x }
  Nothing  -> Just $ k{ kbDesc = "[!!!]" ++  d, kbMaybeAction = skip }

externalBindings :: ThreadState -> DBusState -> [KeyGroup FeatureX]
externalBindings ts db =
  [ KeyGroup "Launchers"
    [ KeyBinding "<XF86Search>" "select/launch app" $ Left runAppMenu
    , KeyBinding "M-g" "launch clipboard manager" $ Left runClipMenu
    , KeyBinding "M-a" "launch network selector" $ Left runNetMenu
    , KeyBinding "M-w" "launch window selector" $ Left runWinMenu
    , KeyBinding "M-u" "launch device selector" $ Left runDevMenu
    , KeyBinding "M-b" "launch bitwarden selector" $ Left runBwMenu
    , KeyBinding "M-v" "launch ExpressVPN selector" $ Left runVPNMenu
    , KeyBinding "M-e" "launch bluetooth selector" $ Left runBTMenu
    , KeyBinding "M-C-e" "launch editor" $ Left runEditor
    , KeyBinding "M-C-w" "launch browser" $ Left runBrowser
    , KeyBinding "M-C-t" "launch terminal with tmux" $ Left runTMux
    , KeyBinding "M-C-S-t" "launch terminal" $ Left runTerm
    , KeyBinding "M-C-q" "launch calc" $ Left runCalc
    , KeyBinding "M-C-f" "launch file manager" $ Left runFileManager
    ]

  , KeyGroup "Actions"
    [ KeyBinding "M-q" "close window" $ ftrAlways "kill window function" kill1
    , KeyBinding "M-r" "run program" $ Left runCmdMenu
    , KeyBinding "M-<Space>" "warp pointer" $ ftrAlways "warp function" $ warpToWindow 0.5 0.5
    , KeyBinding "M-C-s" "capture area" $ Left runAreaCapture
    , KeyBinding "M-C-S-s" "capture screen" $ Left runScreenCapture
    , KeyBinding "M-C-d" "capture desktop" $ Left runDesktopCapture
    , KeyBinding "M-C-b" "browse captures" $ Left runCaptureBrowser
    -- , ("M-C-S-s", "capture focused window", spawn myWindowCap)
    ]

  , KeyGroup "Multimedia"
    [ KeyBinding "<XF86AudioPlay>" "toggle play/pause" $ Left runTogglePlay
    , KeyBinding "<XF86AudioPrev>" "previous track" $ Left runPrevTrack
    , KeyBinding "<XF86AudioNext>" "next track" $ Left runNextTrack
    , KeyBinding "<XF86AudioStop>" "stop" $ Left runStopPlay
    , KeyBinding "<XF86AudioLowerVolume>" "volume down" $ Left runVolumeDown
    , KeyBinding "<XF86AudioRaiseVolume>" "volume up" $ Left runVolumeUp
    , KeyBinding "<XF86AudioMute>" "volume mute" $ Left runVolumeMute
    ]

  , KeyGroup "Dunst"
    [ KeyBinding "M-`" "dunst history" $ Left runNotificationHistory
    , KeyBinding "M-S-`" "dunst close" $ Left runNotificationClose
    , KeyBinding "M-M1-`" "dunst context menu" $ Left runNotificationContext
    , KeyBinding "M-C-`" "dunst close all" $ Left runNotificationCloseAll
    ]

  , KeyGroup "System"
    [ KeyBinding "M-." "backlight up" $ ib bctlInc
    , KeyBinding "M-," "backlight down" $ ib bctlDec
    , KeyBinding "M-M1-," "backlight min" $ ib bctlMin
    , KeyBinding "M-M1-." "backlight max" $ ib bctlMax
    , KeyBinding "M-S-." "keyboard up" $ ck bctlInc
    , KeyBinding "M-S-," "keyboard down" $ ck bctlDec
    , KeyBinding "M-S-M1-," "keyboard min" $ ck bctlMin
    , KeyBinding "M-S-M1-." "keyboard max" $ ck bctlMax
    , KeyBinding "M-<End>" "power menu" $ Left runPowerPrompt
    , KeyBinding "M-<Home>" "quit xmonad" $ Left runQuitPrompt
    , KeyBinding "M-<Delete>" "lock screen" $ Left runScreenLock
    -- M-<F1> reserved for showing the keymap
    , KeyBinding "M-<F2>" "restart xmonad" restartf
    , KeyBinding "M-<F3>" "recompile xmonad" recompilef
    , KeyBinding "M-<F7>" "start Isync Service" $ Left runStartISyncService
    , KeyBinding "M-C-<F7>" "start Isync Timer" $ Left runStartISyncTimer
    , KeyBinding "M-<F8>" "select autorandr profile" $ Left runAutorandrMenu
    , KeyBinding "M-<F9>" "toggle ethernet" $ Left runToggleEthernet
    , KeyBinding "M-<F10>" "toggle bluetooth" $ Left runToggleBluetooth
    , KeyBinding "M-<F11>" "toggle screensaver" $ Left $ ioSometimes $ callToggle cl
    , KeyBinding "M-<F12>" "switch gpu" $ Left runOptimusPrompt
    ]
  ]
  where
    cl = dbSesClient db
    brightessControls ctl getter = (ioSometimes . getter . ctl) cl
    ib = Left . brightessControls intelBacklightControls
    ck = Left . brightessControls clevoKeyboardControls
    ftrAlways n = Right . Always n . Always_ . FallbackAlone
    restartf = ftrAlways "restart function" (runCleanup ts db >> runRestart)
    recompilef = ftrAlways "recompile function" runRecompile

type MaybeX = Maybe (X ())

type FeatureX = Feature (X ())
