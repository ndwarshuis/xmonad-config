--------------------------------------------------------------------------------
-- | Device plugin
--
-- Display different text depending on whether or not the interface has
-- connectivity

module Xmobar.Plugins.Device
  ( Device(..)
  , devDep
  ) where

import           Control.Monad

import           Data.Internal.DBus
import           Data.Internal.Dependency
import           Data.Word

import           DBus

import           XMonad.Internal.Command.Desktop
import           XMonad.Internal.DBus.Common
import           Xmobar
import           Xmobar.Plugins.Common

newtype Device = Device (String, String, Colors) deriving (Read, Show)

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

devDep :: DBusDependency_ SysClient
devDep = Endpoint networkManagerPkgs networkManagerBus nmPath nmInterface
  $ Method_ getByIP

getDevice :: SysClient -> String -> IO (Maybe ObjectPath)
getDevice sys iface = bodyToMaybe <$> callMethod' sys mc
  where
    mc = (methodCallBus networkManagerBus nmPath nmInterface getByIP)
      { methodCallBody = [toVariant iface]
      }

getDeviceConnected :: ObjectPath -> SysClient -> IO [Variant]
getDeviceConnected path = callPropertyGet networkManagerBus path nmDeviceInterface
  $ memberName_ devSignal

matchStatus :: [Variant] -> SignalMatch Word32
matchStatus = matchPropertyChanged nmDeviceInterface devSignal

instance Exec Device where
  alias (Device (iface, _, _)) = iface
  start (Device (iface, text, colors)) cb = do
    withDBusClientConnection cb $ \sys -> do
      path <- getDevice sys iface
      displayMaybe' cb (listener sys) path
    where
      listener sys path = do
        rule <- matchPropertyFull sys networkManagerBus (Just path)
        -- TODO warn the user here rather than silently drop the listener
        forM_ rule $ \r ->
          startListener r (getDeviceConnected path) matchStatus chooseColor' cb sys
      chooseColor' = return . (\s -> colorText colors s text) . (> 1)
