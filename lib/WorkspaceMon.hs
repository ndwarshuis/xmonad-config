{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WorkspaceMon (M.fromList, runWorkspaceMon) where

import           SendXMsg

import qualified Data.Map                  as M

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

import           System.Directory
import           System.Process            (Pid)

-- TOOD it would be really nice if the manner we used to match windows was
-- the same as that in XMonad itself (eg with Query types)
type MatchTags = M.Map String String

type WatchedPIDs = MVar [Pid]

data WConf = WConf
    { display   :: Display
    , matchTags :: MatchTags
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

runWorkspaceMon :: MatchTags -> IO ()
runWorkspaceMon mts = do
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  curPIDs <- newMVar [] -- TODO this is ugly, use a mutable state monad
  -- listen only for substructure change events (which includes MapNotify)
  allocaSetWindowAttributes $ \a -> do
    set_event_mask a substructureNotifyMask
    changeWindowAttributes dpy root cWEventMask a
  let c = WConf { display = dpy, matchTags = mts }
  _ <- allocaXEvent $ \e ->
    runW c $ forever $ handle curPIDs =<< io (nextEvent dpy e >> getEvent e)
  return ()

handle :: WatchedPIDs -> Event -> W ()

-- | assume this fires at least once when a new window is created (also could
-- use CreateNotify but that is really noisy)
handle curPIDs MapNotifyEvent { ev_window = w } = do
  dpy <- asks display
  hint <- io $ getClassHint dpy w
  mts <- asks matchTags
  let tag = M.lookup (resClass hint) mts
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
waitAndKill tag pid = waitUntilExit pidDir
  where
    pidDir = "/proc/" ++ show pid
    waitUntilExit d = do
      -- TODO this will not work if the process is a zombie (maybe I care...)
      -- ASSUMPTION on linux PIDs will always increase until they overflow, in
      -- which case they will start to recycle. Barring any fork bombs, this
      -- code should work because we can reasonably expect that no processes
      -- will spawn with the same PID within the delay limit
      res <- doesDirectoryExist d
      if res then threadDelay 100000 >> waitUntilExit d
      else sendXMsg Workspace tag

withUniquePid :: WatchedPIDs -> Pid -> IO () -> IO ()
withUniquePid curPIDs pid f = do
  pids <- readMVar curPIDs
  unless (pid `elem` pids) $ do
    modifyMVar_ curPIDs (return . (pid:))
    f
    modifyMVar_ curPIDs (return . filter (/=pid))
