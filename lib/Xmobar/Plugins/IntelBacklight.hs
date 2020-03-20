{-# LANGUAGE LambdaCase #-}

module Xmobar.Plugins.IntelBacklight where

import Control.Concurrent
import Control.Monad

import DBus.IntelBacklight

import Xmobar

newtype IntelBacklight = IntelBacklight String deriving (Read, Show)

instance Exec IntelBacklight where
  alias (IntelBacklight _) = "intelbacklight"
  start (IntelBacklight icon) cb = do
    _ <- matchSignal $ cb . formatBrightness
    cb . formatBrightness =<< callGetBrightness
    forever (threadDelay 5000)
    where
      formatBrightness = \case
        Just b  -> icon ++ show (b `div` 100) ++ "%"
        Nothing -> "N/A"
