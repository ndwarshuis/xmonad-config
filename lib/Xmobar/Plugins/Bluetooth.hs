{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Xmobar.Plugins.Bluetooth where

import Control.Concurrent
import Control.Monad

import Data.List  (find)
import Data.Maybe (mapMaybe)

import DBus
import DBus.Client

import Xmobar

newtype Bluetooth = Bluetooth (String, String, String) deriving (Read, Show)

rule :: MatchRule
rule = matchAny
  { matchPath = Just "/org/bluez/hci0"
  , matchInterface = Just "org.freedesktop.DBus.Properties"
  , matchMember = Just "PropertiesChanged"
  }

instance Exec Bluetooth where
  alias (Bluetooth _) = "bluetooth"
  start (Bluetooth (text, colorOn, colorOff)) cb = do
    client <- connectSystem
    _ <- addMatch client rule $ cb . fmtState . stateFromSignal
    -- TODO initialize here
    -- cb . formatBrightness =<< callGetBrightness
    forever (threadDelay 5000)
    where
      -- TODO this is total utter garbage...but it works...
      stateFromSignal sig = join
          $ fmap (fromVariant :: (Variant -> Maybe Bool))
          $ join
          $ fmap (fromVariant :: (Variant -> Maybe Variant))
          $ fmap snd
          $ find (\(k, _) -> (fromVariant k :: Maybe String) == Just "Powered")
          $ concatMap dictionaryItems
          $ mapMaybe fromVariant
          $ filter (\v -> variantType v == TypeDictionary TypeString TypeVariant)
          $ signalBody sig
      fmtState = \case
        Just s -> wrapColor text $ if s then colorOn else colorOff
        Nothing -> "N/A"
      wrapColor s c = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"
