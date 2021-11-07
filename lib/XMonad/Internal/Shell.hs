--------------------------------------------------------------------------------
-- | Functions for formatting and spawning shell commands

module XMonad.Internal.Shell
  ( fmtCmd
  , spawnCmd
  , doubleQuote
  , singleQuote
  , skip
  , (#!&&)
  , (#!||)
  , (#!|)
  , (#!>>)
  ) where

import           Control.Monad.IO.Class

import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Opening subshell

spawnCmd :: MonadIO m => String -> [String] -> m ()
spawnCmd cmd args = spawn $ fmtCmd cmd args

--------------------------------------------------------------------------------
-- | Formatting commands

fmtCmd :: String -> [String] -> String
fmtCmd cmd args = unwords $ cmd : args

(#!&&) :: String -> String -> String
cmdA #!&& cmdB = cmdA ++ " && " ++ cmdB

infixr 0 #!&&

(#!|) :: String -> String -> String
cmdA #!| cmdB = cmdA ++ " | " ++ cmdB

infixr 0 #!|

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

skip :: Monad m => m ()
skip = return ()
