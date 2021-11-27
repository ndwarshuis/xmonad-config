module Xmobar.Plugins.Device
  ( Device(..)
  , devDep
  ) where

--------------------------------------------------------------------------------
-- | Devince plugin
--
-- Display different text depending on whether or not the interface has
-- connectivity

import           Control.Monad

import           Data.Maybe
import           Data.Word

import           DBus
import           DBus.Client
import           DBus.Internal

import           XMonad.Internal.Dependency
import           Xmobar
import           Xmobar.Plugins.Common

newtype Device = Device (String, String, String, String) deriving (Read, Show)

nmBus :: BusName
nmBus = busName_ "org.freedesktop.NetworkManager"

nmPath :: ObjectPath
nmPath = objectPath_ "/org/freedesktop/NetworkManager"

nmInterface :: InterfaceName
nmInterface = interfaceName_ "org.freedesktop.NetworkManager"

nmDeviceInterface :: InterfaceName
nmDeviceInterface = interfaceName_ "org.freedesktop.NetworkManager.Device"

getByIP :: MemberName
getByIP = memberName_ "GetDeviceByIpIface"

devSignal :: String
devSignal = "Ip4Connectivity"

devDep :: DBusDep
devDep = Endpoint nmBus nmPath nmInterface $ Method_ getByIP

getDevice :: Client -> String -> IO (Maybe ObjectPath)
getDevice client iface = either (const Nothing) (fromVariant <=< listToMaybe)
  <$> callMethod' client mc
  where
    mc = (methodCall nmPath nmInterface getByIP)
      { methodCallBody = [toVariant iface]
      , methodCallDestination = Just nmBus
      }

getDeviceConnected :: ObjectPath -> Client -> IO [Variant]
getDeviceConnected path = callPropertyGet nmBus path nmDeviceInterface devSignal

matchStatus :: [Variant] -> SignalMatch Word32
matchStatus = matchPropertyChanged nmDeviceInterface devSignal

instance Exec Device where
  alias (Device (iface, _, _, _)) = iface
  start (Device (iface, text, colorOn, colorOff)) cb = do
    withDBusClientConnection True cb $ \c -> do
      path <- getDevice c iface
      maybe (cb na) (listener c) path
    where
      listener client path = startListener (matchProperty path)
        (getDeviceConnected path) matchStatus chooseColor' cb client
      chooseColor' = return . chooseColor text colorOn colorOff . (> 1)
