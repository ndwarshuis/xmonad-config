--------------------------------------------------------------------------------
-- | Functions for formatting and spawning shell commands

module XMonad.Internal.Shell
  ( MaybeExe(..)
  , Dependency(..)
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

import           Data.Maybe              (isJust)

import           System.Directory        (findExecutable)
import           System.FilePath.Posix

import           XMonad.Core             (X, getXMonadDir)
import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Gracefully handling missing binaries

data Dependency = Required String | Optional String deriving (Eq, Show)

data MaybeExe m = Installed (m ()) [String] | Missing [Dependency] | Ignore

type MaybeX = MaybeExe X

type IOMaybeX = IO MaybeX

exeInstalled :: String -> IO Bool
exeInstalled x = isJust <$> findExecutable x

depInstalled :: Dependency -> IO Bool
depInstalled (Required d) = exeInstalled d
depInstalled (Optional d) = exeInstalled d

filterMissing :: [Dependency] -> IO [Dependency]
filterMissing = filterM (fmap not . depInstalled)

runIfInstalled :: MonadIO m => [Dependency] -> m () -> IO (MaybeExe m)
runIfInstalled ds x = do
  missing <- filterMissing ds
  return $ if null [m | Required m <- missing]
    then Installed x [m | Optional m <- missing]
    else Missing missing

spawnIfInstalled :: MonadIO m => String -> IO (MaybeExe m)
spawnIfInstalled exe = runIfInstalled [Required exe] $ spawn exe

spawnCmdIfInstalled :: MonadIO m => String -> [String] -> IO (MaybeExe m)
spawnCmdIfInstalled exe args = runIfInstalled [Required exe] $ spawnCmd exe args

whenInstalled :: Monad m => MaybeExe m -> m ()
whenInstalled (Installed x _) = x
whenInstalled _               = return ()

skip :: Monad m => m ()
skip = return ()

noCheck :: Monad m => a () -> m (MaybeExe a)
noCheck = return . flip Installed []

--------------------------------------------------------------------------------
-- | Opening subshell

spawnCmd :: MonadIO m => String -> [String] -> m ()
spawnCmd cmd args = spawn $ fmtCmd cmd args

--------------------------------------------------------------------------------
-- | Playing sound

soundDir :: FilePath
soundDir = "sound"

spawnSound :: MonadIO m => FilePath -> m () -> m () -> IO (MaybeExe m)
spawnSound file pre post = runIfInstalled [Required "paplay"]
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
