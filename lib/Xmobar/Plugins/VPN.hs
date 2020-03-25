{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Xmobar.Plugins.VPN where

import           DBus
import           DBus.Client

import           Xmobar
import           Xmobar.Common

data VPN = VPN (String, String, String) Int
    deriving (Read, Show)

callConnectionType :: Client -> IO (Either MethodError Variant)
callConnectionType client =
  getProperty client (methodCall "/org/freedesktop/NetworkManager"
                      "org.freedesktop.NetworkManager" "PrimaryConnectionType")
    { methodCallDestination = Just "org.freedesktop.NetworkManager" }

instance Exec VPN where
  alias (VPN _ _) = "vpn"
  rate  (VPN _ r) = r
  run   (VPN (text, colorOn, colorOff) _) = do
    client <- connectSystem
    reply <- callConnectionType client
    disconnect client
    return $ fmtState $ procReply reply
    where
      procReply = \case
        Right r -> (fromVariant r :: Maybe String)
        Left _  -> Nothing
      fmtState = \case
        Just s -> wrapColor (if s == "vpn" then colorOn else colorOff) text
        Nothing -> "N/A"
