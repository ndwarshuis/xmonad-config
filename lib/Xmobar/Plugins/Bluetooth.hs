--------------------------------------------------------------------------------
-- | Bluetooth plugin
--
-- Use the bluez interface on DBus to check status

module Xmobar.Plugins.Bluetooth
  ( Bluetooth(..)
  , btAlias
  , btDep
  ) where

import           Data.Maybe

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Common
import           XMonad.Internal.Dependency
import           Xmobar
import           Xmobar.Plugins.Common

newtype Bluetooth = Bluetooth (String, String, String) deriving (Read, Show)


btInterface :: InterfaceName
btInterface = interfaceName_ "org.bluez.Adapter1"

-- weird that this is a string when introspecting but a member name when calling
-- a method, not sure if it is supposed to work like that
btPowered :: String
btPowered = "Powered"

btBus :: BusName
btBus = busName_ "org.bluez"

-- TODO this feels like something that shouldn't be hardcoded
btPath :: ObjectPath
btPath = objectPath_ "/org/bluez/hci0"

btAlias :: String
btAlias = "bluetooth"

btDep :: DBusDep
btDep = Endpoint btBus btPath btInterface $ Property_ btPowered

matchPowered :: [Variant] -> SignalMatch Bool
matchPowered = matchPropertyChanged btInterface btPowered fromVariant

callGetPowered :: Client -> IO [Variant]
callGetPowered = callPropertyGet btBus btPath btInterface btPowered

instance Exec Bluetooth where
  alias (Bluetooth _) = btAlias
  start (Bluetooth (text, colorOn, colorOff)) cb = do
    withDBusClientConnection_ True $ \c -> do
      reply <- callGetPowered c
      cb $ maybe "N/A" chooseColor' $ fromVariant =<< listToMaybe reply
      addMatchCallback (matchProperty btPath) (procMatch cb . matchPowered) c
    where
      procMatch f (Match on) = f $ chooseColor' on
      procMatch f Failure    = f "N/A"
      procMatch _ NoMatch    = return ()
      chooseColor' = chooseColor text colorOn colorOff
