--------------------------------------------------------------------------------
-- | Bluetooth plugin
--
-- Use the bluez interface on DBus to check status
--
-- org.bluez dynamically updates its DBus interfaces using the standard Object
-- Manager. The adapter is located at path "/org/bluez/hci<X>" where X is
-- usually 0, and each device is "/org/bluez/hci<X>/<MAC_ADDRESS>".
--
-- This plugin will reflect if the adapter is powered and if any device is
-- connected to it. The rough outline for this procedure:
-- 1) get the adapter from the object manager
-- 2) get all devices associated with the adapter using the object interface
-- 3) determine if the adapter is powered
-- 4) determine if any devices are connected
-- 5) format the icon; powered vs not powered controls the color and connected
--    vs not connected controls the icon (connected bluetooth symbol has two
--    dots flanking it)
--
-- Step 3 can be accomplished using the "org.bluez.Adapter1" interface and
-- querying the "Powered" property. Step 4 can be done using the
-- "org.bluez.Device1" interface and the "Connected" property for each device
-- path. Since these are properties, we can asynchronously read changes to them
-- via the "PropertiesChanged" signal.
--
-- If any devices are added/removed, steps 2-4 will need to be redone and any
-- listeners will need to be updated. (TODO not sure which signals to use in
-- determining if a device is added)
--
-- TODO also not sure if I need to care about multiple adapters and/or the
-- adapter changing.

module Xmobar.Plugins.Bluetooth
  ( Bluetooth(..)
  , btAlias
  , btDep
  ) where

import           Control.Concurrent.MVar
import           Control.Monad

import           Data.List
import           Data.List.Split
import qualified Data.Map                    as M
import           Data.Maybe

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Common
import           XMonad.Internal.Dependency
import           Xmobar
import           Xmobar.Plugins.Common

btAlias :: String
btAlias = "bluetooth"

btDep :: DBusDep
btDep = Endpoint btBus btOmPath omInterface $ Method_ getManagedObjects

data Bluetooth = Bluetooth Icons Colors deriving (Read, Show)

instance Exec Bluetooth where
  alias (Bluetooth _ _) = btAlias
  start (Bluetooth icons colors) cb =
    withDBusClientConnection True cb $ startAdapter icons colors cb

startAdapter :: Icons -> Colors -> Callback -> Client -> IO ()
startAdapter is cs cb cl = do
  ot <- getBtObjectTree cl
  state <- newMVar emptyState
  let display = displayIcon cb (iconFormatter is cs) state
  forM_ (findAdapter ot) $ \adapter -> do
    -- set up adapter
    initAdapter state adapter cl
    addAdaptorListener state display adapter cl
    -- set up devices on the adapter (and listeners for adding/removing devices)
    let devices = findDevices adapter ot
    addDeviceAddedListener state display adapter cl
    addDeviceRemovedListener state display adapter cl
    forM_ devices $ \d -> addAndInitDevice state display d cl
    -- after setting things up, show the icon based on the initialized state
    display

--------------------------------------------------------------------------------
-- | Icon Display
--
-- Color corresponds to the adaptor powered state, and the icon corresponds to
-- if it is paired or not. If the adaptor state is undefined, display "N/A"

type IconFormatter = (Maybe Bool -> Bool -> String)

type Icons = (String, String)

type Colors = (String, String)

displayIcon :: Callback -> IconFormatter -> MutableBtState -> IO ()
displayIcon callback formatter =
  callback . uncurry formatter <=< readState

-- TODO maybe I want this to fail when any of the device statuses are Nothing
iconFormatter :: Icons -> Colors -> IconFormatter
iconFormatter (iconConn, iconDisc) (colorOn, colorOff) powered connected =
  maybe na (chooseColor icon colorOn colorOff) powered
  where
    icon = if connected then iconConn else iconDisc

--------------------------------------------------------------------------------
-- | Connection State
--
-- The signal handlers all run on separate threads, yet the icon depends on
-- the state reflected by all these signals. The best (only?) way to do this is
-- is to track the shared state of the bluetooth adaptor and its devices using
-- an MVar.

data BTDevice = BTDevice
  { btDevConnected  :: Maybe Bool
  , btDevSigHandler :: SignalHandler
  }

type ConnectedDevices = M.Map ObjectPath BTDevice

data BtState = BtState
  { btDevices :: ConnectedDevices
  , btPowered :: Maybe Bool
  }

type MutableBtState = MVar BtState

emptyState :: BtState
emptyState = BtState
  { btDevices = M.empty
  , btPowered = Nothing
  }

readState :: MutableBtState -> IO (Maybe Bool, Bool)
readState state = do
  p <- readPowered state
  c <- readDevices state
  return (p, anyDevicesConnected c)

--------------------------------------------------------------------------------
-- | Object manager

findAdapter :: ObjectTree -> Maybe ObjectPath
findAdapter = find (("/org/bluez/hci" `isPrefixOf`) . formatObjectPath) . M.keys

findDevices :: ObjectPath -> ObjectTree -> [ObjectPath]
findDevices adapter = filter (adaptorHasDevice adapter) . M.keys

adaptorHasDevice :: ObjectPath -> ObjectPath -> Bool
adaptorHasDevice adaptor device = case splitPath device of
  [org, bluez, hciX, _] -> splitPath adaptor == [org, bluez, hciX]
  _                     -> False

splitPath :: ObjectPath -> [String]
splitPath = splitOn "/" . dropWhile (=='/') . formatObjectPath

getBtObjectTree :: Client -> IO ObjectTree
getBtObjectTree client = callGetManagedObjects client btBus btOmPath

btBus :: BusName
btBus = busName_ "org.bluez"

btOmPath :: ObjectPath
btOmPath = objectPath_ "/"

addDeviceAddedListener :: MutableBtState -> IO () -> ObjectPath -> Client -> IO ()
addDeviceAddedListener state display adapter client =
  addInterfaceAddedListener btOmPath addDevice client
  where
    addDevice = pathCallback adapter display $ \d ->
      addAndInitDevice state display d client

addDeviceRemovedListener :: MutableBtState -> IO () -> ObjectPath -> Client -> IO ()
addDeviceRemovedListener state display adapter client =
  addInterfaceRemovedListener btOmPath remDevice client
  where
    remDevice = pathCallback adapter display $ \d -> do
      old <- removeDevice state d
      forM_ old $ removeMatch client . btDevSigHandler

pathCallback :: ObjectPath -> IO () -> (ObjectPath -> IO ()) -> SignalCallback
pathCallback adapter display f [device, _] = forM_ (fromVariant device) $ \d ->
  when (adaptorHasDevice adapter d) $ f d >> display
pathCallback _ _ _ _ = return ()

--------------------------------------------------------------------------------
-- | Adapter

initAdapter :: MutableBtState -> ObjectPath -> Client -> IO ()
initAdapter state adapter client = do
  reply <- callGetPowered adapter client
  putPowered state $ fromSingletonVariant reply

addAdaptorListener :: MutableBtState -> IO () -> ObjectPath -> Client -> IO ()
addAdaptorListener state display adaptor =
  void . addMatchCallback rule (procMatch . matchPowered)
  where
    rule = matchProperty adaptor
    procMatch = withSignalMatch $ \b -> putPowered state b >> display

callGetPowered :: ObjectPath -> Client -> IO [Variant]
callGetPowered adapter =
  callPropertyGet btBus adapter adapterInterface adaptorPowered

matchPowered :: [Variant] -> SignalMatch Bool
matchPowered = matchPropertyChanged adapterInterface adaptorPowered

putPowered :: MutableBtState -> Maybe Bool -> IO ()
putPowered m ds = modifyMVar_ m (\s -> return s { btPowered = ds })

readPowered :: MutableBtState -> IO (Maybe Bool)
readPowered = fmap btPowered . readMVar

adapterInterface :: InterfaceName
adapterInterface = interfaceName_ "org.bluez.Adapter1"

adaptorPowered :: String
adaptorPowered = "Powered"

--------------------------------------------------------------------------------
-- | Devices

addAndInitDevice :: MutableBtState -> IO () -> ObjectPath -> Client -> IO ()
addAndInitDevice state display device client = do
  sh <- addDeviceListener state display device client
  initDevice state sh device client

initDevice :: MutableBtState -> SignalHandler -> ObjectPath -> Client -> IO ()
initDevice state sh device client = do
  reply <- callGetConnected device client
  void $ insertDevice state device $
    BTDevice { btDevConnected = fromVariant =<< listToMaybe reply
             , btDevSigHandler = sh
             }

addDeviceListener :: MutableBtState -> IO () -> ObjectPath -> Client -> IO SignalHandler
addDeviceListener state display device =
  addMatchCallback rule (procMatch . matchConnected)
  where
    rule = matchProperty device
    procMatch = withSignalMatch $ \c -> updateDevice state device c >> display

matchConnected :: [Variant] -> SignalMatch Bool
matchConnected = matchPropertyChanged devInterface devConnected

callGetConnected :: ObjectPath -> Client -> IO [Variant]
callGetConnected p = callPropertyGet btBus p devInterface devConnected

insertDevice :: MutableBtState -> ObjectPath -> BTDevice -> IO Bool
insertDevice m device dev = modifyMVar m $ \s -> do
  let new = M.insert device dev $ btDevices s
  return (s { btDevices = new }, anyDevicesConnected new)

updateDevice :: MutableBtState -> ObjectPath -> Maybe Bool -> IO Bool
updateDevice m device status = modifyMVar m $ \s -> do
  let new = M.update (\d -> Just d { btDevConnected = status }) device $ btDevices s
  return (s { btDevices = new }, anyDevicesConnected new)

anyDevicesConnected :: ConnectedDevices -> Bool
anyDevicesConnected = or . mapMaybe btDevConnected . M.elems

removeDevice :: MutableBtState -> ObjectPath -> IO (Maybe BTDevice)
removeDevice m device = modifyMVar m $ \s -> do
  let devs = btDevices s
  return (s { btDevices = M.delete device devs }, M.lookup device devs)

readDevices :: MutableBtState -> IO ConnectedDevices
readDevices = fmap btDevices . readMVar

devInterface :: InterfaceName
devInterface = interfaceName_ "org.bluez.Device1"

devConnected :: String
devConnected = "Connected"
