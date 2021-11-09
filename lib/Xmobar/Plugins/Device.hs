{-# LANGUAGE OverloadedStrings #-}

module Xmobar.Plugins.Device
  ( Device(..)
  , devBus
  , devPath
  , devInterface
  , devGetByIP
  ) where

-- TOOD this name can be more general
--------------------------------------------------------------------------------
-- | Ethernet plugin
--
-- Display different text depending on whether or not the interface has
-- connectivity


import           Control.Monad

import           Data.Word

import           DBus
import           DBus.Client

import           XMonad.Hooks.DynamicLog (xmobarColor)
import           Xmobar

data Device = Device (String, String, String, String) Int
    deriving (Read, Show)

devBus :: BusName
devBus = "org.freedesktop.NetworkManager"

devPath :: ObjectPath
devPath = "/org/freedesktop/NetworkManager"

devInterface :: InterfaceName
devInterface = "org.freedesktop.NetworkManager"

devGetByIP :: MemberName
devGetByIP = "GetDeviceByIpIface"

getDevice :: Client -> String -> IO (Maybe ObjectPath)
getDevice client iface = do
  let mc = methodCall devPath devInterface devGetByIP
  reply <- call client $ mc { methodCallBody = [toVariant iface]
                            , methodCallDestination = Just devBus
                            }
  return $ case reply of
    Left _  -> Nothing
    Right b -> case methodReturnBody b of
      [objectPath] -> fromVariant objectPath
      _            -> Nothing

getDeviceConnected :: Client -> ObjectPath -> IO (Maybe Bool)
getDeviceConnected client objectPath = do
  let mc = methodCall objectPath
           "org.freedesktop.NetworkManager.Device"
           "Ip4Connectivity"
  either (const Nothing) (fmap ((> 1) :: Word32 -> Bool) . fromVariant)
    <$> getProperty client mc { methodCallDestination = Just devBus }

instance Exec Device where
  alias (Device (iface, _, _, _) _) = iface
  rate  (Device _ r) = r
  run   (Device (iface, text, colorOn, colorOff) _) = do
    client <- connectSystem
    dev <- getDevice client iface
    state <- join <$> mapM (getDeviceConnected client) dev
    disconnect client
    return $ maybe "N/A" fmt state
    where
      fmt s = xmobarColor (if s then colorOn else colorOff) "" text
