--------------------------------------------------------------------------------
-- | Common internal DBus functions

module XMonad.Internal.DBus.Common
  ( addMatchCallback
  , xmonadBusName
  ) where

import           DBus
import           DBus.Client

xmonadBusName :: BusName
xmonadBusName = busName_ "org.xmonad"

-- | Bind a callback to a signal match rule
addMatchCallback :: MatchRule -> ([Variant] -> IO ()) -> IO SignalHandler
addMatchCallback rule cb = do
  client <- connectSession
  addMatch client rule $ cb . signalBody
