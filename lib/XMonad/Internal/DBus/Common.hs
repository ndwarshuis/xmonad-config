--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Common
  ( xmonadBusName
  , btBus
  ) where

import           DBus

xmonadBusName :: BusName
xmonadBusName = busName_ "org.xmonad"

btBus :: BusName
btBus = busName_ "org.bluez"

