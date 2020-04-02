module XMonad.Internal.Shell
  ( fmtCmd
  , spawnCmd
  , (#!&&)
  , (#!||)
  , (#!>>)
  ) where

import           XMonad

fmtCmd :: String -> [String] -> String
fmtCmd cmd args = unwords $ cmd : args

spawnCmd :: String -> [String] -> X ()
spawnCmd cmd args = spawn $ fmtCmd cmd args

(#!&&) :: String -> String -> String
cmdA #!&& cmdB = cmdA ++ " && " ++ cmdB

infixr 0 #!&&

(#!||) :: String -> String -> String
cmdA #!|| cmdB = cmdA ++ " || " ++ cmdB

infixr 0 #!||

(#!>>) :: String -> String -> String
cmdA #!>> cmdB = cmdA ++ "; " ++ cmdB

infixr 0 #!>>
