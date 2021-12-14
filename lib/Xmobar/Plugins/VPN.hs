--------------------------------------------------------------------------------
-- | VPN plugin
--
-- Use the networkmanager to detect when a VPN interface is added or removed.
-- Specifically, monitor the object tree to detect paths with the interface
-- "org.freedesktop.NetworkManager.Device.Tun".

module Xmobar.Plugins.VPN
  ( VPN(..)
  , vpnAlias
  , vpnDep
  ) where

import           Control.Concurrent.MVar
import           Control.Monad

import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Set                   as S

import           DBus
import           DBus.Client
import           DBus.Internal

import           XMonad.Internal.Dependency
import           Xmobar
import           Xmobar.Plugins.Common

newtype VPN = VPN (String, Colors) deriving (Read, Show)

instance Exec VPN where
  alias (VPN _) = vpnAlias
  start (VPN (text, colors)) cb =
    withDBusClientConnection True cb $ \c -> do
    state <- initState c
    let display = displayMaybe cb iconFormatter . Just =<< readState state
    let signalCallback' f = f state display
    vpnAddedListener (signalCallback' addedCallback) c
    vpnRemovedListener (signalCallback' removedCallback) c
    display
    where
      iconFormatter b = return $ colorText colors b text

--------------------------------------------------------------------------------
-- | VPN State
--
-- Maintain a set of paths which are the currently active VPNs. Most of the time
-- this will be a null or singleton set, but this setup could handle the edge
-- case of multiple VPNs being active at once without puking.

type VPNState = S.Set ObjectPath

type MutableVPNState = MVar VPNState

initState :: Client -> IO MutableVPNState
initState client = do
  ot <- getVPNObjectTree client
  newMVar $ findTunnels ot

readState :: MutableVPNState -> IO Bool
readState = fmap (not . null) . readMVar

updateState :: (ObjectPath -> VPNState -> VPNState) -> MutableVPNState
  -> ObjectPath -> IO ()
updateState f state op = modifyMVar_ state $ return . f op

--------------------------------------------------------------------------------
-- | Tunnel Device Detection
--

getVPNObjectTree :: Client -> IO ObjectTree
getVPNObjectTree client = callGetManagedObjects client vpnBus vpnPath

findTunnels :: ObjectTree -> VPNState
findTunnels = S.fromList . M.keys . M.filter (elem vpnDeviceTun . M.keys)

vpnAddedListener :: SignalCallback -> Client -> IO ()
vpnAddedListener = fmap void . addInterfaceAddedListener vpnBus vpnPath

vpnRemovedListener :: SignalCallback -> Client -> IO ()
vpnRemovedListener = fmap void . addInterfaceRemovedListener vpnBus vpnPath

addedCallback :: MutableVPNState -> IO () -> SignalCallback
addedCallback state display [device, added] = update >> display
  where
    added' = fromVariant added :: Maybe (M.Map String (M.Map String Variant))
    is = M.keys $ fromMaybe M.empty added'
    update = updateDevice S.insert state device is
addedCallback _ _ _                       = return ()

removedCallback :: MutableVPNState -> IO () -> SignalCallback
removedCallback state display [device, interfaces] = update >> display
  where
    is = fromMaybe [] $ fromVariant interfaces :: [String]
    update = updateDevice S.delete state device is
removedCallback _ _ _                       = return ()

updateDevice :: (ObjectPath -> VPNState -> VPNState) -> MutableVPNState
  -> Variant -> [String] -> IO ()
updateDevice f state device interfaces = when (vpnDeviceTun `elem` interfaces) $
  forM_ d $ updateState f state
  where
    d = fromVariant device :: Maybe ObjectPath

--------------------------------------------------------------------------------
-- | DBus Interface
--

vpnBus :: BusName
vpnBus = busName_ "org.freedesktop.NetworkManager"

vpnPath :: ObjectPath
vpnPath = objectPath_ "/org/freedesktop"

vpnDeviceTun :: String
vpnDeviceTun = "org.freedesktop.NetworkManager.Device.Tun"

vpnAlias :: String
vpnAlias = "vpn"

vpnDep :: DBusDep
vpnDep = Endpoint vpnBus vpnPath omInterface $ Method_ getManagedObjects
