{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Bluetooth plugin
--
-- Use the bluez interface on DBus to check status

module Xmobar.Plugins.Bluetooth
  ( Bluetooth(..)
  , btAlias
  , btDep
  ) where


import           DBus
import           DBus.Client

import           XMonad.Hooks.DynamicLog    (xmobarColor)
import           XMonad.Internal.Dependency
import           Xmobar

data Bluetooth = Bluetooth (String, String, String) Int
    deriving (Read, Show)

-- TODO match property signal here
callGetPowered :: Client -> IO (Maybe Variant)
callGetPowered client = either (const Nothing) Just
  <$> getProperty client (methodCall btPath btInterface $ memberName_ btPowered)
    { methodCallDestination = Just btBus }

btInterface :: InterfaceName
btInterface = "org.bluez.Adapter1"

-- weird that this is a string when introspecting but a member name when calling
-- a method, not sure if it is supposed to work like that
btPowered :: String
btPowered = "Powered"

btBus :: BusName
btBus = "org.bluez"

-- TODO this feels like something that shouldn't be hardcoded
btPath :: ObjectPath
btPath = "/org/bluez/hci0"

btAlias :: String
btAlias = "bluetooth"

btDep :: DBusDep
btDep = Endpoint btBus btPath btInterface $ Property_ btPowered

instance Exec Bluetooth where
  alias (Bluetooth _ _) = btAlias
  rate  (Bluetooth _ r) = r
  run   (Bluetooth (text, colorOn, colorOff) _) = do
    client <- connectSystem
    reply <- callGetPowered client
    disconnect client
    return $ fmtState $ fromVariant =<< reply
    where
      fmtState = \case
        Just s  -> xmobarColor (if s then colorOn else colorOff) "" text
        Nothing -> "N/A"
