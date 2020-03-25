module WorkspaceMon (fromList, runWorkspaceMon) where

import           SendXMsg

import           Data.Map                  as M

import           Control.Concurrent
import           Control.Monad

import           Graphics.X11.Types

import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types

import           System.Directory
import           System.Process            (Pid)

-- TODO yes yes I could use a reader monad here...
type MatchTags = Map String String

runWorkspaceMon :: MatchTags -> IO ()
runWorkspaceMon mts = do
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  -- listen only for substructure change events (which includes MapNotify)
  allocaSetWindowAttributes $ \a -> do
    set_event_mask a substructureNotifyMask
    changeWindowAttributes dpy root cWEventMask a
  _ <- allocaXEvent $ \e ->
    forever $ handle dpy mts =<< (nextEvent dpy e >> getEvent e)
  return ()

handle :: Display -> MatchTags -> Event -> IO ()

-- | assume this fires at least once when a new window is created (also could
-- use CreateNotify but that is really noisy)
handle dpy mts MapNotifyEvent { ev_window = w } = do
  hint <- getClassHint dpy w
  -- this will need to eventually accept a conditional argument that
  -- we can change upon initialization
  let tag = M.lookup (resClass hint) mts
  case tag of
    Just t -> do
      a <- internAtom dpy "_NET_WM_PID" False
      pid <- getWindowProperty32 dpy a w
      case pid of
        -- ASSUMPTION windows will only have one PID at one time
        Just [p] -> waitAndKill t $ fromIntegral p
        _        -> return ()
    Nothing -> return ()

handle _ _ _ = return ()

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
      if res then do
        threadDelay 100000
        waitUntilExit d
      else do
        print "sending"
        sendXMsg "%%%%%" tag
