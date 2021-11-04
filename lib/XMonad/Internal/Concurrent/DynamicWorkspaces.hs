{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- | Automatically Manage Dynamic Workspaces
-- This is a somewhat convoluted wrapper for the Dymamic Workspaces module
-- in the contrib library. The general behavior this allows:
-- 1) launch app
-- 2) move app to its own dynamic workspace
-- 3) close app and remove dynamic workspace
--
-- The only sane way to do this is to monitor the lifetime of a PID on a dynamic
-- workspace (effectively tying each dynamic workspace to a single PID). Xmonad
-- is single threaded and thus cannot "wait" for PIDs to exit, so this spawns
-- a separate thread outside XMonad that will in turn spawn monitor threads
-- for each dynamic workspace. When these monitor threads detect that the app
-- has closed, they will send an event to X which can be caught by Xmonad so
-- the workspace can be removed.
--
-- What is the motivation? Some apps suck and don't play nice with others on
-- normal workspaces, so I would rather have them go in their own little
-- environment and misbehave.
--
-- Examples:
-- 1) Gimp (lots of trays and floating windows)
-- 2) Xsane (see Gimp)
-- 3) Virtualbox (should always be by itself anyways)

module XMonad.Internal.Concurrent.DynamicWorkspaces
  ( DynWorkspace(..)
  , appendShift
  , appendViewShift
  , removeDynamicWorkspace
  , runWorkspaceMon
  , spawnOrSwitch
  , doSink
  ) where

import           Data.List                                (deleteBy, find)
import qualified Data.Map                                 as M
import           Data.Maybe

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader

import           Graphics.X11.Types

import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types

import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Core
    ( ManageHook
    , WorkspaceId
    , X
    , withWindowSet
    )
import           XMonad.Hooks.ManageHelpers               (MaybeManageHook)
import           XMonad.Internal.Concurrent.ClientMessage
import           XMonad.Internal.Process
import           XMonad.ManageHook
import           XMonad.Operations
import qualified XMonad.StackSet                          as W

--------------------------------------------------------------------------------
-- | Dynamic Workspace datatype
-- This hold all the data needed to tie an app to a particular dynamic workspace

data DynWorkspace = DynWorkspace
    { dwName  :: String
    , dwTag   :: WorkspaceId
    , dwClass :: String
    , dwHook  :: [MaybeManageHook]
    , dwKey   :: Char
    , dwCmd   :: Maybe (X ())
    -- TODO this should also have the layout for this workspace
    }

--------------------------------------------------------------------------------
-- | Manager thread
-- The main thread that watches for new windows. When a match is found, this
-- thread spawns a new thread the waits for the PID of the window to exit. When
-- the PID exits, it sends a ClientMessage event to X

-- TOOD it would be really nice if the manner we used to match windows was
-- the same as that in XMonad itself (eg with Query types)
-- type MatchTags = M.Map String String

type WatchedPIDs = MVar [Pid]

data WConf = WConf
    { display       :: Display
    , dynWorkspaces :: [DynWorkspace]
    }

newtype W a = W (ReaderT WConf IO a)
  deriving (Functor, Monad, MonadIO, MonadReader WConf)

instance Applicative W where
  pure = return
  (<*>) = ap

runW :: WConf -> W a -> IO a
runW c (W a) = runReaderT a c

io :: MonadIO m => IO a -> m a
io = liftIO

runWorkspaceMon :: [DynWorkspace] -> IO ()
runWorkspaceMon dws = do
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  curPIDs <- newMVar [] -- TODO this is ugly, use a mutable state monad
  -- listen only for substructure change events (which includes MapNotify)
  allocaSetWindowAttributes $ \a -> do
    set_event_mask a substructureNotifyMask
    changeWindowAttributes dpy root cWEventMask a
  let c = WConf { display = dpy, dynWorkspaces = dws }
  _ <- allocaXEvent $ \e ->
    runW c $ forever $ handle curPIDs =<< io (nextEvent dpy e >> getEvent e)
  return ()

handle :: WatchedPIDs -> Event -> W ()

-- | assume this fires at least once when a new window is created (also could
-- use CreateNotify but that is really noisy)
handle curPIDs MapNotifyEvent { ev_window = w } = do
  dpy <- asks display
  hint <- io $ getClassHint dpy w
  dws <- asks dynWorkspaces
  let m = M.fromList $ fmap (\DynWorkspace { dwTag = t, dwClass = c } -> (c, t)) dws
  let tag = M.lookup (resClass hint) m
  io $ forM_ tag $ \t -> do
    a <- internAtom dpy "_NET_WM_PID" False
    pid <- getWindowProperty32 dpy a w
    case pid of
      -- ASSUMPTION windows will only have one PID at one time
      Just [p] -> let p' = fromIntegral p
        in void $ forkIO $ withUniquePid curPIDs p' $ waitAndKill t p'
      _        -> return ()

handle _ _ = return ()

waitAndKill :: String -> Pid -> IO ()
waitAndKill tag pid = waitUntilExit pid >> sendXMsg Workspace tag

withUniquePid :: WatchedPIDs -> Pid -> IO () -> IO ()
withUniquePid curPIDs pid f = do
  pids <- readMVar curPIDs
  unless (pid `elem` pids) $ do
    modifyMVar_ curPIDs (return . (pid:))
    f
    modifyMVar_ curPIDs (return . filter (/=pid))

--------------------------------------------------------------------------------
-- | Launching apps
-- When launching apps on dymamic workspaces, first check if they are running
-- and launch if not, then switch to their workspace

wsOccupied :: Eq a1 => a1 -> W.StackSet a1 l a2 sid sd -> Bool
wsOccupied tag ws = elem tag $ map W.tag $ filter (isJust . W.stack)
  -- list of all workspaces with windows on them
  -- TODO is there not a better way to do this?
  $ W.workspace (W.current ws) : W.hidden ws ++ map W.workspace (W.visible ws)

spawnOrSwitch :: WorkspaceId -> X () -> X ()
spawnOrSwitch tag cmd = do
  occupied <- withWindowSet $ return . wsOccupied tag
  if occupied then windows $ W.view tag else cmd

--------------------------------------------------------------------------------
-- | Managehook
-- Move windows to new workspace if they are part of a dynamic workspace

-- shamelessly ripped off from appendWorkspace (this analogue doesn't exist)
appendHiddenWorkspace :: String -> X ()
appendHiddenWorkspace = addHiddenWorkspaceAt (flip (++) . return)

viewShift :: WorkspaceId -> ManageHook
viewShift = doF . liftM2 (.) W.view W.shift

-- NOTE: need to appendHidden because the regular append function will shift
-- to the new workspace, which I don't want for this one
appendShift :: String -> ManageHook
appendShift tag = liftX (appendHiddenWorkspace tag) >> doF (W.shift tag)

appendViewShift :: String -> ManageHook
appendViewShift tag = liftX (appendWorkspace tag) >> viewShift tag

-- TODO surprisingly this doesn't exist? We shouldn't need to TBH
doSink :: ManageHook
doSink = doF $ \s -> case W.stack $ W.workspace $ W.current s of
                       Just s' -> W.sink (W.focus s') s
                       Nothing -> s

--------------------------------------------------------------------------------
-- | Eventhook
-- When an app is closed, this will respond the event that is sent in the main
-- XMonad thread

removeDynamicWorkspace :: WorkspaceId -> X ()
removeDynamicWorkspace target = windows removeIfEmpty
  where
    -- remove workspace if it is empty and if there are hidden workspaces
    removeIfEmpty s@W.StackSet { W.visible = vis, W.hidden = hall@(h:hs) }
      -- if hidden, delete from hidden
      | Just x <- find isEmptyTarget hall
      = s { W.hidden = deleteBy (eq W.tag) x hall }
      -- if visible, delete from visible and move first hidden to its place
      | Just x <- find (isEmptyTarget . W.workspace) vis
      = s { W.visible = x { W.workspace = h } : deleteBy (eq W.screen) x vis
          , W.hidden = hs }
      -- if current, move the first hidden workspace to the current
      | isEmptyTarget $ W.workspace $ W.current s
      = s { W.current = (W.current s) { W.workspace = h }, W.hidden = hs }
      -- otherwise do nothing
      | otherwise = s
    removeIfEmpty s = s
    isEmptyTarget ws = isNothing (W.stack ws) && W.tag ws == target
    eq f x y = f x == f y
