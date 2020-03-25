module WorkspaceMon (runWorkspaceMon) where

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

runWorkspaceMon :: IO ()
runWorkspaceMon = do
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  -- listen only for substructure change events (which includes MapNotify)
  allocaSetWindowAttributes $ \a -> do
    set_event_mask a substructureNotifyMask
    changeWindowAttributes dpy root cWEventMask a
  _ <- allocaXEvent $ \e ->
    forever $ handle dpy =<< (nextEvent dpy e >> getEvent e)
  return ()

handle :: Display -> Event -> IO ()

-- | assume this fires at least once when a new window is created (also could
-- use CreateNotify but that is really noisy)
handle dpy MapNotifyEvent { ev_window = w } = do
  hint <- getClassHint dpy w
  -- this will need to eventually accept a conditional argument that
  -- we can change upon initialization
  when (resName hint == "gimp") $ do
    a <- internAtom dpy "_NET_WM_PID" False
    pid <- getWindowProperty32 dpy a w
    case pid of
      -- ASSUMPTION windows will only have one PID at one time
      Just [p] -> waitAndKill $ fromIntegral p
      _        -> return ()

handle _ _ = return ()

waitAndKill :: Pid -> IO ()
waitAndKill pid = waitUntilExit pidDir
  where
    pidDir = "/proc/" ++ show pid
    waitUntilExit d = do
      -- TODO this will not work if the process is a zombie (maybe I care...)
      -- ASSUMPTION on linux PIDs will always increase until they overflow, in
      -- which case they will start to recycle. Barring any fork bombs, this
      -- code should work because we can reasonably expect that no processes
      -- will spawn with the same PID within the delay limit
      res <- doesDirectoryExist d
      when res $ do
          threadDelay 100000
          waitUntilExit d
