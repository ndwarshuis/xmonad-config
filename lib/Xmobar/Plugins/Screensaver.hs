{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- | Screensaver plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.Screensaver (Screensaver(..)) where

import           Control.Concurrent
import           Control.Monad

import           Xmobar

import           XMonad.Hooks.DynamicLog          (xmobarColor)
import           XMonad.Internal.DBus.Screensaver

newtype Screensaver = Screensaver (String, String, String)
    deriving (Read, Show)

instance Exec Screensaver where
    alias (Screensaver _) = "screensaver"
    start (Screensaver (text, colorOn, colorOff)) cb = do
      _ <- matchSignal $ cb . fmtState
      cb . fmtState =<< callQuery
      forever (threadDelay 5000000)
      where
        fmtState = \case
          Just s  -> xmobarColor (if s then colorOn else colorOff) "" text
          Nothing -> "N/A"

