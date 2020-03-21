{-# LANGUAGE OverloadedStrings #-}

module DBus.Internal where

import DBus
import DBus.Client

-- TODO not all methods warrent that we wait for a reply?
callMethod :: MethodCall -> IO (Maybe [Variant])
callMethod mc = do
  client <- connectSession
  -- TODO handle clienterrors here
  reply <- call client mc { methodCallDestination = Just "org.xmonad" }
  return $ case reply of
    Left _    -> Nothing
    Right ret -> Just $ methodReturnBody ret

addMatchCallback :: MatchRule -> ([Variant] -> IO ()) -> IO SignalHandler
addMatchCallback rule cb = do
  client <- connectSession
  addMatch client rule $ cb . signalBody
