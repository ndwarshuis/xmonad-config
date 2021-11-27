--------------------------------------------------------------------------------
-- | Clevo Keyboard plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.ClevoKeyboard
  ( ClevoKeyboard(..)
  , ckAlias
  ) where

import           Xmobar

import           Xmobar.Plugins.BacklightCommon

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard

newtype ClevoKeyboard = ClevoKeyboard String deriving (Read, Show)

ckAlias :: String
ckAlias = "clevokeyboard"

instance Exec ClevoKeyboard where
  alias (ClevoKeyboard _) = ckAlias
  start (ClevoKeyboard icon) =
     startBacklight matchSignalCK callGetBrightnessCK icon
