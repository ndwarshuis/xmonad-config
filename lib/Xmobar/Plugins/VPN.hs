--------------------------------------------------------------------------------
-- | VPN plugin
--
-- Use the NetworkManger interface on DBus to check status

module Xmobar.Plugins.VPN
  ( VPN(..)
  , vpnAlias
  , vpnDep
  ) where

import           Data.Maybe

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Common
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

matchConnType :: [Variant] -> SignalMatch String
matchConnType = matchPropertyChanged vpnInterface vpnConnType fromVariant

callGetConnectionType :: Client -> IO [Variant]
callGetConnectionType = callPropertyGet vpnBus vpnPath vpnInterface vpnConnType

instance Exec VPN where
  alias (VPN _) = vpnAlias
  start (VPN (text, colorOn, colorOff)) cb = do
    withDBusClientConnection_ True $ \c -> do
      reply <- callGetConnectionType c
      cb $ maybe "N/A" chooseColor' $ fromVariant =<< listToMaybe reply
      addMatchCallback (matchProperty vpnPath) (procMatch cb . matchConnType) c
    where
      procMatch f (Match t) = f $ chooseColor' t
      procMatch f Failure   = f "N/A"
      procMatch _ NoMatch   = return ()
      chooseColor' = chooseColor text colorOn colorOff . ("vpn" ==)
