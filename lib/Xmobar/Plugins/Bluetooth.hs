{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Bluetooth plugin
--
-- Use the bluez interface on DBus to check status

module Xmobar.Plugins.Bluetooth (Bluetooth(..)) where

import           DBus
import           DBus.Client

import           XMonad.Hooks.DynamicLog (xmobarColor)
import           Xmobar

data Bluetooth = Bluetooth (String, String, String) Int
    deriving (Read, Show)

callGetPowered :: Client -> IO (Either MethodError Variant)
callGetPowered client =
  getProperty client (methodCall "/org/bluez/hci0" "org.bluez.Adapter1" "Powered")
    { methodCallDestination = Just "org.bluez" }

instance Exec Bluetooth where
  alias (Bluetooth _ _) = "bluetooth"
  rate  (Bluetooth _ r) = r
  run   (Bluetooth (text, colorOn, colorOff) _) = do
    client <- connectSystem
    reply <- callGetPowered client
    disconnect client
    return $ fmtState $ procReply reply
    where
      procReply = \case
        -- TODO handle errors?
        Right r -> fromVariant r
        Left _  -> Nothing
      fmtState = \case
        Just s  -> xmobarColor (if s then colorOn else colorOff) "" text
        Nothing -> "N/A"
