--------------------------------------------------------------------------------
-- | Functions for formatting and spawning shell commands

module XMonad.Internal.Shell
  ( fmtCmd
  , spawnCmd
  , spawnSound
  , (#!&&)
  , (#!||)
  , (#!>>)
  ) where

import           System.FilePath.Posix

import           XMonad.Core             (X, getXMonadDir)
import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Opening subshell

spawnCmd :: String -> [String] -> X ()
spawnCmd cmd args = spawn $ fmtCmd cmd args

--------------------------------------------------------------------------------
-- | Playing sound

soundDir :: FilePath
soundDir = "sound"

spawnSound :: FilePath -> X ()
spawnSound file = do
  path <- (</> soundDir </> file) <$> getXMonadDir
  spawnCmd "aplay" [path]

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
