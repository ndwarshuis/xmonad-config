{-# LANGUAGE LambdaCase #-}

module Xmobar.Plugins.Screensaver where

import Control.Concurrent
import Control.Monad

import DBus.Screensaver

import Xmobar

data Screensaver = Screensaver (String, String, String)
    deriving (Read, Show)

instance Exec Screensaver where
    alias (Screensaver _) = "screensaver"
    start (Screensaver (text, colorOn, colorOff)) cb = do
      _ <- matchSignal $ cb . fmtState
      cb . fmtState =<< callQuery
      forever (threadDelay 5000000)
      where
        fmtState = \case
          Just s -> wrapColor text $ if s then colorOn else colorOff
          Nothing -> "N/A"
        wrapColor s c = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

