{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | VPN plugin
--
-- Use the NetworkManger interface on DBus to check status

module Xmobar.Plugins.VPN
  ( VPN(..)
  , vpnAlias
  , vpnBus
  , vpnPath
  ) where

import           DBus
import           DBus.Client

import           XMonad.Hooks.DynamicLog (xmobarColor)
import           Xmobar

data VPN = VPN (String, String, String) Int
    deriving (Read, Show)

callConnectionType :: Client -> IO (Either MethodError Variant)
callConnectionType client =
  getProperty client (methodCall vpnPath
                      "org.freedesktop.NetworkManager" "PrimaryConnectionType")
    { methodCallDestination = Just vpnBus }

vpnBus :: BusName
vpnBus = "org.freedesktop.NetworkManager"

vpnPath :: ObjectPath
vpnPath = "/org/freedesktop/NetworkManager"

vpnAlias :: String
vpnAlias = "vpn"

instance Exec VPN where
  alias (VPN _ _) = vpnAlias
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
        Just s -> xmobarColor (if s == "vpn" then colorOn else colorOff) "" text
        Nothing -> "N/A"
