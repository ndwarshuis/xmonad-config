module Xmobar.Backlight where

import Control.Monad
import Control.Concurrent

import Data.Int (Int32)

import DBus
import DBus.Client

import DBus.Backlight

import Xmobar

data Backlight = Backlight (String, String)
  deriving (Read, Show)

instance Exec Backlight where
    alias (Backlight _) = "betterbacklight"
    start (Backlight _) cb = do
      -- print "connecting"
      client <- connectSession
      _ <- addMatch client brMatcher $ \sig -> do
        cb $ formatSignal sig
        -- print sig
      forever (threadDelay 5000)

formatSignal :: Signal -> String
formatSignal sig = -- show $ (map fromVariant $ signalBody sig :: [Maybe Int32])
  case signalBody sig of
    [] -> "N/A"
    (x:_) -> case (fromVariant x :: Maybe Int32) of
      Just i -> show i
      Nothing -> "n/a"
