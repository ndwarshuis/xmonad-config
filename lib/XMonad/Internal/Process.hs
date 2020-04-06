--------------------------------------------------------------------------------
-- | Functions for managing processes

module XMonad.Internal.Process
  ( waitUntilExit
  , killHandle
  , spawnPipe
  , createProcess'
  , proc'
  , shell'
  , spawn
  , spawnAt
  , module System.Process
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad

import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix.Signals
import           System.Process

import           XMonad.Core          hiding (spawn)

-- | Block until a PID has exited (in any form)
-- ASSUMPTION on linux PIDs will always increase until they overflow, in which
-- case they will start to recycle. Barring any fork bombs, this code should
-- work because we can reasonably expect that no processes will spawn with the
-- same PID within the delay limit
-- TODO this will not work if the process is a zombie (maybe I care...)
waitUntilExit :: Show t => t -> IO ()
waitUntilExit pid = do
  res <- doesDirectoryExist $ "/proc/" ++ show pid
  when res $ threadDelay 100000 >> waitUntilExit pid

killHandle :: ProcessHandle -> IO ()
killHandle ph = do
  pid <- getPid ph
  forM_ pid $ signalProcess sigTERM
  -- this may fail if the process exits instantly and the handle
  -- is destroyed by the time we get to this line (I think?)
  void (try $ waitForProcess ph :: IO (Either IOException ExitCode))

withDefaultSignalHandlers :: IO a -> IO a
withDefaultSignalHandlers =
  bracket_ uninstallSignalHandlers installSignalHandlers

addGroupSession :: CreateProcess -> CreateProcess
addGroupSession cp = cp { create_group = True, new_session = True }

createProcess' :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess' = withDefaultSignalHandlers . createProcess

shell' :: String -> CreateProcess
shell' = addGroupSession . shell

proc' :: FilePath -> [String] -> CreateProcess
proc' cmd args = addGroupSession $ proc cmd args

spawn :: String -> X ()
spawn = io . void . createProcess' . shell'

spawnAt :: FilePath -> String -> X ()
spawnAt fp cmd = io $ void $ createProcess' $ (shell' cmd) { cwd = Just fp }

spawnPipe :: String -> IO (Handle, ProcessHandle)
spawnPipe cmd = do
  -- ASSUME creating a pipe will always succeed in making a Just Handle
  (Just h, _, _, p) <- createProcess' $ (shell cmd) { std_in = CreatePipe }
  hSetBuffering h LineBuffering
  return (h, p)
