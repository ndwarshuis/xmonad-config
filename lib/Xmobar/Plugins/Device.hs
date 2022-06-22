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

import           Data.Word

import           DBus
import           DBus.Client
import           DBus.Internal

import           XMonad.Internal.Dependency
import           Xmobar
import           Xmobar.Plugins.Common

newtype Device = Device (String, String, Colors) deriving (Read, Show)

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

devDep :: DBusDependency p
devDep = Endpoint nmBus nmPath nmInterface $ Method_ getByIP

getDevice :: Client -> String -> IO (Maybe ObjectPath)
getDevice client iface = bodyToMaybe <$> callMethod' client mc
  where
    mc = (methodCallBus nmBus nmPath nmInterface getByIP)
      { methodCallBody = [toVariant iface]
      }

getDeviceConnected :: ObjectPath -> Client -> IO [Variant]
getDeviceConnected path = callPropertyGet nmBus path nmDeviceInterface
  $ memberName_ devSignal

matchStatus :: [Variant] -> SignalMatch Word32
matchStatus = matchPropertyChanged nmDeviceInterface devSignal

instance Exec Device where
  alias (Device (iface, _, _)) = iface
  start (Device (iface, text, colors)) cb = do
    withDBusClientConnection True cb $ \client -> do
      path <- getDevice client iface
      displayMaybe' cb (listener client) path
    where
      listener client path = do
        rule <- matchPropertyFull client nmBus (Just path)
        -- TODO warn the user here rather than silently drop the listener
        forM_ rule $ \r ->
          startListener r (getDeviceConnected path) matchStatus chooseColor' cb client
      chooseColor' = return . (\s -> colorText colors s text) . (> 1)
