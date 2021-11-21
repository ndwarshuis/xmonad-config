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

import           DBus.Client

import           Xmobar

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard

newtype ClevoKeyboard = ClevoKeyboard String deriving (Read, Show)

ckAlias :: String
ckAlias = "clevokeyboard"

instance Exec ClevoKeyboard where
  alias (ClevoKeyboard _) = ckAlias
  start (ClevoKeyboard icon) cb = do
    _ <- matchSignalCK $ cb . formatBrightness
    -- TODO this could fail, and also should try to reuse client objects when
    -- possible
    cb . formatBrightness =<< callGetBrightnessCK =<< connectSession
    forever (threadDelay 5000000)
    where
      formatBrightness = \case
        Just b  -> icon ++ show (round b :: Integer) ++ "%"
        Nothing -> "N/A"
