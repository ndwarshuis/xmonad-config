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

module WorkspaceMon
  ( DynWorkspace(..)
  , appendViewShift
  , removeDynamicWorkspace
  , runWorkspaceMon
  , spawnOrSwitch
  )
where

import           Process
import           SendXMsg

import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Semigroup

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

import           System.Process                   (Pid)

import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Core
    ( Query
    , ScreenId
    , WorkspaceId
    , X
    , withWindowSet
    )
import           XMonad.Hooks.ManageHelpers       (MaybeManageHook)
import           XMonad.ManageHook
import           XMonad.Operations
import qualified XMonad.StackSet                  as W

--------------------------------------------------------------------------------
-- | Dynamic Workspace datatype
-- This hold all the data needed to tie an app to a particular dynamic workspace

data DynWorkspace = DynWorkspace
    { dwName  :: String
    , dwTag   :: WorkspaceId
    , dwClass :: String
    , dwHook  :: [MaybeManageHook]
    , dwCmd   :: Maybe (String, X ())
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

viewShift
  :: WorkspaceId -> Query (Endo (W.StackSet WorkspaceId l Window ScreenId sd))
viewShift = doF . liftM2 (.) W.view W.shift

appendViewShift
  :: String -> Query (Endo (W.StackSet WorkspaceId l Window ScreenId sd))
appendViewShift tag = liftX (appendWorkspace tag) >> viewShift tag

--------------------------------------------------------------------------------
-- | Eventhook
-- When an app is closed, this will respond the event that is sent in the main
-- XMonad thread

removeDynamicWorkspace :: WorkspaceId -> X ()
removeDynamicWorkspace = removeEmptyWorkspaceByTag
