--------------------------------------------------------------------------------
-- | Functions for formatting and spawning shell commands

module XMonad.Internal.Shell
  ( fmtCmd
  , spawnCmd
  , (#!&&)
  , (#!||)
  , (#!>>)
  ) where

import           XMonad.Core             (X)
import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Opening subshell

spawnCmd :: String -> [String] -> X ()
spawnCmd cmd args = spawn $ fmtCmd cmd args

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
