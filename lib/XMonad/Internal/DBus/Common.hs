{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Common internal DBus functions

module XMonad.Internal.DBus.Common
  ( callMethod
  , addMatchCallback
  ) where

import           DBus
import           DBus.Client

-- TODO export the bus name (org.xmonad)

-- | Call a method and return its result if successful
callMethod :: MethodCall -> IO (Maybe [Variant])
callMethod mc = do
  client <- connectSession
  -- TODO handle clienterrors here
  reply <- call client mc { methodCallDestination = Just "org.xmonad" }
  -- TODO not all methods warrent that we wait for a reply?
  return $ case reply of
    Left _    -> Nothing
    Right ret -> Just $ methodReturnBody ret

-- | Bind a callback to a signal match rule
addMatchCallback :: MatchRule -> ([Variant] -> IO ()) -> IO SignalHandler
addMatchCallback rule cb = do
  client <- connectSession
  s <- addMatch client rule $ cb . signalBody
  disconnect client
  return s
