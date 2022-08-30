{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- | Start a VirtualBox instance with a sentinel wrapper process.
--
-- The only reason why this is needed is because I want to manage virtualboxes
-- in their own dynamic workspaces, which are currently set up to correspond to
-- one process. The problem with Virtualbox is that the VBoxManage command
-- spawns a new VM and then exits, which means the process that was originally
-- attached to the dynamic workspace only exists for a few seconds when the VM
-- is starting.
--
-- Solution: Run VBoxManage in a wrapper binary that launches the VM and sleeps
-- until its PID exits. By monitoring this wrapper, the dynamic workspace only
-- has one process to track and will maintain the workspace throughout the
-- lifetime of the VM.

module Main (main) where

import           Control.Exception

import           Data.List

import           Text.Read
import           Text.XML.Light

import           System.Environment
import           System.Exit

import           XMonad.Internal.Concurrent.VirtualBox
import           XMonad.Internal.Process

main :: IO ()
main = runAndWait =<< getArgs

runAndWait :: [String] -> IO ()
runAndWait [n] = either putStrLn runConfig =<< vmInstanceConfig n
  where
    runConfig c = maybe err runID =<< vmMachineID c
    runID i = do
      vmLaunch i
      p <- vmPID i
      waitUntilExit p
    err = putStrLn "Could not get machine ID"

runAndWait _ = putStrLn "Usage: vbox-start VBOXNAME"

vmLaunch :: String -> IO ()
vmLaunch i = do
  (rc, _, _) <- readCreateProcessWithExitCode' cmd ""
  case rc of
    ExitSuccess -> return ()
    _           -> putStrLn $ "Failed to start VM: " ++ i
  where
    cmd = proc "VBoxManage" ["startvm", i]

vmPID :: String -> IO (Maybe Int)
vmPID vid = do
  (rc, out, _) <- readCreateProcessWithExitCode' cmd ""
  return $ case rc of
    ExitSuccess -> readMaybe out
    _           -> Nothing
  where
    cmd = proc "pgrep" ["-f", "VirtualBoxVM.*" ++ vid]

vmMachineID :: FilePath -> IO (Maybe String)
vmMachineID iPath = do
  (s :: Either IOException String) <- try $ readFile iPath
  return $ case s of
    (Left _)  -> Nothing
    (Right x) -> findMachineID =<< parseXMLDoc x
  where
    findMachineID e = stripPrefix "{"
      =<< (fmap reverse . stripPrefix "}" . reverse)
      =<< findAttr (blank_name { qName = "uuid" })
      =<< findChild (qual e "Machine") e

