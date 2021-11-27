--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Common
  ( xmonadBusName
  ) where

import           DBus

xmonadBusName :: BusName
xmonadBusName = busName_ "org.xmonad"
