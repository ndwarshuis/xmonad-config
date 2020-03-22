{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Xmobar.Plugins.NetworkManager where

import DBus
import DBus.Client

import Xmobar

data NetworkManager = NetworkManager (String, String, String) Int
    deriving (Read, Show)

callConnectionType :: Client -> IO (Either MethodError Variant)
callConnectionType client =
  getProperty client (methodCall "/org/freedesktop/NetworkManager"
                      "org.freedesktop.NetworkManager" "PrimaryConnectionType")
    { methodCallDestination = Just "org.freedesktop.NetworkManager" }

instance Exec NetworkManager where
  alias (NetworkManager _ _) = "networkmanager"
  rate  (NetworkManager _ r) = r
  run   (NetworkManager (text, colorOn, colorOff) _) = do
    client <- connectSystem
    reply <- callConnectionType client
    disconnect client
    return $ fmtState $ procReply reply
    where
      procReply = \case
        Right r -> (fromVariant r :: Maybe String)
        Left _  -> Nothing
      fmtState = \case
        Just s -> wrapColor text $ if s == "vpn" then colorOn else colorOff
        Nothing -> "N/A"
      wrapColor s c = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"
