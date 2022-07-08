--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Common
  ( xmonadBusName
  , btBus
  , notifyBus
  , notifyPath
  , networkManagerBus
  ) where

import           DBus

xmonadBusName :: BusName
xmonadBusName = busName_ "org.xmonad"

btBus :: BusName
btBus = busName_ "org.bluez"

notifyBus :: BusName
notifyBus = busName_ "org.freedesktop.Notifications"

notifyPath :: ObjectPath
notifyPath = objectPath_ "/org/freedesktop/Notifications"

networkManagerBus :: BusName
networkManagerBus = busName_ "org.freedesktop.NetworkManager"

