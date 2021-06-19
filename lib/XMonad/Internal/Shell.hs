--------------------------------------------------------------------------------
-- | Functions for formatting and spawning shell commands

module XMonad.Internal.Shell
  ( MaybeExe(..)
  , MaybeX
  , IOMaybeX
  , runIfInstalled
  , whenInstalled
  , spawnIfInstalled
  , spawnCmdIfInstalled
  , noCheck
  , fmtCmd
  , spawnCmd
  , spawnSound
  , playSound
  , doubleQuote
  , singleQuote
  , skip
  , (#!&&)
  , (#!||)
  , (#!>>)
  ) where

import           Control.Monad           (filterM)
import           Control.Monad.IO.Class

import           Data.Maybe              (isNothing)

import           System.FilePath.Posix
import           System.Directory        (findExecutable)

import           XMonad.Core             (getXMonadDir, X)
import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Gracefully handling missing binaries

data MaybeExe m = Installed (m ()) | Missing [String] | Noop

type MaybeX = MaybeExe X

type IOMaybeX = IO MaybeX

runIfInstalled :: MonadIO m => [String] -> m () -> IO (MaybeExe m)
runIfInstalled exes x = do
  missing <- filterM (fmap isNothing . findExecutable) exes
  return $ case missing of
    [] -> Installed x
    ms -> Missing ms

spawnIfInstalled :: MonadIO m => String -> IO (MaybeExe m)
spawnIfInstalled exe = runIfInstalled [exe] $ spawn exe

spawnCmdIfInstalled :: MonadIO m => String -> [String] -> IO (MaybeExe m)
spawnCmdIfInstalled exe args = runIfInstalled [exe] $ spawnCmd exe args

whenInstalled :: Monad m => MaybeExe m -> m ()
whenInstalled (Installed x) = x
whenInstalled _ = return ()

skip :: Monad m => m ()
skip = return ()

noCheck :: Monad m => a () -> m (MaybeExe a)
noCheck = return . Installed

--------------------------------------------------------------------------------
-- | Opening subshell

spawnCmd :: MonadIO m => String -> [String] -> m ()
spawnCmd cmd args = spawn $ fmtCmd cmd args

--------------------------------------------------------------------------------
-- | Playing sound

soundDir :: FilePath
soundDir = "sound"

spawnSound :: MonadIO m => FilePath -> m () -> m () -> IO (MaybeExe m)
spawnSound file pre post = runIfInstalled ["paplay"]
  $ pre >> playSound file >> post

playSound :: MonadIO m => FilePath -> m ()
playSound file = do
  path <- (</> soundDir </> file) <$> getXMonadDir
  -- paplay seems to have less latency than aplay
  spawnCmd "paplay" [path]

--------------------------------------------------------------------------------
-- | Formatting commands

fmtCmd :: String -> [String] -> String
fmtCmd cmd args = unwords $ cmd : args

(#!&&) :: String -> String -> String
cmdA #!&& cmdB = cmdA ++ " && " ++ cmdB

infixr 0 #!&&

(#!||) :: String -> String -> String
cmdA #!|| cmdB = cmdA ++ " || " ++ cmdB

infixr 0 #!||

(#!>>) :: String -> String -> String
cmdA #!>> cmdB = cmdA ++ "; " ++ cmdB

infixr 0 #!>>

doubleQuote :: String -> String
doubleQuote s = "\"" ++ s ++ "\""

singleQuote :: String -> String
singleQuote s = "'" ++ s ++ "'"
