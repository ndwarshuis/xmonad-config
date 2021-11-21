{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- | Clevo Keyboard plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.ClevoKeyboard
  ( ClevoKeyboard(..)
  , ckAlias
  ) where

import           Control.Concurrent
import           Control.Monad

import           Xmobar

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard

data ClevoKeyboard = ClevoKeyboard String
  deriving (Read, Show)

ckAlias :: String
ckAlias = "clevokeyboard"

instance Exec ClevoKeyboard where
  alias (ClevoKeyboard _) = ckAlias
  start (ClevoKeyboard icon) cb = do
    _ <- matchSignalCK $ cb . formatBrightness
    cb . formatBrightness =<< callGetBrightnessCK
    forever (threadDelay 5000000)
    where
      formatBrightness = \case
        Just b  -> icon ++ show (round b :: Integer) ++ "%"
        Nothing -> "N/A"
