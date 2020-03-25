{-# LANGUAGE LambdaCase #-}

module Xmobar.Plugins.Screensaver where

import           Control.Concurrent
import           Control.Monad

import           DBus.Screensaver

import           Xmobar
import           Xmobar.Common

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
          Just s -> wrapColor (if s then colorOn else colorOff) text
          Nothing -> "N/A"

