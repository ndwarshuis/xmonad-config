--------------------------------------------------------------------------------
-- | Intel backlight plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.IntelBacklight
  ( IntelBacklight(..)
  , blAlias
  ) where

import           Xmobar

import           Xmobar.Plugins.BacklightCommon

import           XMonad.Internal.DBus.Brightness.IntelBacklight

newtype IntelBacklight = IntelBacklight String deriving (Read, Show)

blAlias :: String
blAlias = "intelbacklight"

instance Exec IntelBacklight where
  alias (IntelBacklight _) = blAlias
  start (IntelBacklight icon) =
     startBacklight matchSignalIB callGetBrightnessIB icon
