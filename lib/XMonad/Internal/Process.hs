{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- | Functions for managing processes

module XMonad.Internal.Process
  ( waitUntilExit
  , killPID
  , spawnPipe'
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix.IO
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types
import           System.Process           hiding (createPipe)
import           System.Process.Internals
    ( ProcessHandle__ (ClosedHandle, OpenHandle)
    , mkProcessHandle
    , withProcessHandle
    )

import           XMonad.Core

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

killPID :: ProcessID -> IO ()
killPID pid = do
  h <- mkProcessHandle pid False
  -- this may fail of the PID does not exist
  _ <- try $ sendSIGTERM h :: IO (Either IOException ())
  -- this may fail if the process exits instantly and the handle
  -- is destroyed by the time we get to this line (I think?)
  _ <- try $ waitForProcess h :: IO (Either IOException ExitCode)
  return ()
  where
    sendSIGTERM h = withProcessHandle h $ \case
      OpenHandle _ -> signalProcess sigTERM pid
      ClosedHandle _ -> return ()
      _ -> return () -- this should never happen

spawnPipe' :: MonadIO m => String -> m (ProcessID, Handle)
spawnPipe' x = liftIO $ do
  (rd, wr) <- createPipe
  setFdOption wr CloseOnExec True
  h <- fdToHandle wr
  hSetBuffering h LineBuffering
  p <- xfork $ do
    _ <- dupTo rd stdInput
    executeFile "/bin/sh" False ["-c", x] Nothing
  closeFd rd
  return (p, h)
