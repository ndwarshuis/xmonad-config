{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- | Intel backlight plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.IntelBacklight
  ( IntelBacklight(..)
  , blAlias
  ) where

import           Control.Concurrent
import           Control.Monad

import           Xmobar

import           XMonad.Internal.DBus.IntelBacklight

newtype IntelBacklight = IntelBacklight String deriving (Read, Show)

blAlias :: String
blAlias = "intelbacklight"

instance Exec IntelBacklight where
  alias (IntelBacklight _) = blAlias
  start (IntelBacklight icon) cb = do
    _ <- matchSignal $ cb . formatBrightness
    cb . formatBrightness =<< callGetBrightness
    forever (threadDelay 5000000)
    where
      formatBrightness = \case
        Just b  -> icon ++ show (round $ b / 100 :: Integer) ++ "%"
        Nothing -> "N/A"
