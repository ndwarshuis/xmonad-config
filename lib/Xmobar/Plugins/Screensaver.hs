{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- | Screensaver plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.Screensaver
  ( Screensaver(..)
  , ssAlias
  ) where

import           Control.Concurrent
import           Control.Monad

import           DBus.Client

import           Xmobar

import           XMonad.Hooks.DynamicLog          (xmobarColor)
import           XMonad.Internal.DBus.Screensaver

newtype Screensaver = Screensaver (String, String, String) deriving (Read, Show)

ssAlias :: String
ssAlias = "screensaver"

instance Exec Screensaver where
    alias (Screensaver _) = ssAlias
    start (Screensaver (text, colorOn, colorOff)) cb = do
      _ <- matchSignal $ cb . fmtState
      cb . fmtState =<< callQuery =<< connectSession
      forever (threadDelay 5000000)
      where
        fmtState = \case
          Just s  -> xmobarColor (if s then colorOn else colorOff) "" text
          Nothing -> "N/A"

