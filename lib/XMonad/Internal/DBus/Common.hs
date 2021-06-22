--------------------------------------------------------------------------------
-- | Common internal DBus functions

module XMonad.Internal.DBus.Common
  ( callMethod
  , callMethod'
  , addMatchCallback
  , xmonadBus
  ) where

import           DBus
import           DBus.Client

xmonadBus :: BusName
xmonadBus = busName_ "org.xmonad"

-- | Call a method and return its result if successful
callMethod :: MethodCall -> IO (Maybe [Variant])
callMethod mc = do
  client <- connectSession
  r <- callMethod' client (Just xmonadBus) mc
  disconnect client
  return r

callMethod' :: Client -> Maybe BusName -> MethodCall -> IO (Maybe [Variant])
callMethod' client bn mc = do
  -- TODO handle clienterrors here
  reply <- call client mc { methodCallDestination = bn }
  -- TODO not all methods warrant that we wait for a reply? (see callNoReply)
  return $ case reply of
    Left _    -> Nothing
    Right ret -> Just $ methodReturnBody ret

-- | Bind a callback to a signal match rule
addMatchCallback :: MatchRule -> ([Variant] -> IO ()) -> IO SignalHandler
addMatchCallback rule cb = do
  client <- connectSession
  addMatch client rule $ cb . signalBody
