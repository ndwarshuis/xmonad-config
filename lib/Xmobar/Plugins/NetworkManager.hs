{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Xmobar.Plugins.NetworkManager where

import Control.Concurrent
import Control.Monad

import DBus
import DBus.Client
import DBus.Internal.Types

import Xmobar

newtype NetworkManager =  NetworkManager (String, String, String)
  deriving (Read, Show)

rule :: MatchRule
rule = matchAny
  { matchInterface = Just "org.freedesktop.NetworkManager.VPN.Connection"
  , matchMember = Just "VpnStateChanged"
  }

-- TODO would polling be better for this? Using events means that we need
-- to catch all of them perfectly to stay synchronized...which *might* happen

instance Exec NetworkManager where
  alias (NetworkManager _) = "networkmanager"
  start (NetworkManager (text, colorOn, colorOff)) cb = do
  -- start (NetworkManager _) cb = do
    client <- connectSystem
    -- TODO initialize
    _ <- addMatch client rule $ cb . fmtState . getVPNState . signalBody
    forever (threadDelay 5000000)
    where
      getVPNState = \case
        [Variant (ValueAtom (AtomWord32 s)), _] -> Just s
        _                                       -> Nothing
      fmtState = \case
        -- state = 5 means VPN is connected
        Just s -> wrapColor text $ if s == 5 then colorOn else colorOff
        Nothing -> "N/A"
      wrapColor s c = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"
