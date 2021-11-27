--------------------------------------------------------------------------------
-- | VPN plugin
--
-- Use the NetworkManger interface on DBus to check status

module Xmobar.Plugins.VPN
  ( VPN(..)
  , vpnAlias
  , vpnDep
  ) where

import           DBus
import           DBus.Internal

import           XMonad.Internal.Dependency
import           Xmobar
import           Xmobar.Plugins.Common

newtype VPN = VPN (String, String, String) deriving (Read, Show)

vpnBus :: BusName
vpnBus = busName_ "org.freedesktop.NetworkManager"

vpnPath :: ObjectPath
vpnPath = objectPath_ "/org/freedesktop/NetworkManager"

vpnInterface :: InterfaceName
vpnInterface = interfaceName_ "org.freedesktop.NetworkManager"

vpnConnType :: String
vpnConnType = "PrimaryConnectionType"

vpnAlias :: String
vpnAlias = "vpn"

vpnDep :: DBusDep
vpnDep = Endpoint vpnBus vpnPath vpnInterface $ Property_ vpnConnType

instance Exec VPN where
  alias (VPN _) = vpnAlias
  start (VPN (text, colorOn, colorOff)) cb =
    withDBusClientConnection True cb
    $ startListener rule getProp fromSignal chooseColor' cb
    where
      rule = matchProperty vpnPath
      getProp = callPropertyGet vpnBus vpnPath vpnInterface vpnConnType
      fromSignal = matchPropertyChanged vpnInterface vpnConnType
      chooseColor' = return . chooseColor text colorOn colorOff . ("vpn" ==)
