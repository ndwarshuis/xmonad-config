{-# LANGUAGE OverloadedStrings #-}

module DBus.Internal where

import Control.Monad (forM_)

import DBus
import DBus.Client

callMethod' :: Client -> MethodCall -> IO (Maybe [Variant])
callMethod' client mc = do
  -- TODO handle clienterrors here
  reply <- call client mc { methodCallDestination = Just "org.xmonad" }
  return $ case reply of
    Left _    -> Nothing
    Right ret -> Just $ methodReturnBody ret

callMethod :: MethodCall -> ([Variant] -> Maybe a) -> IO (Maybe a)
callMethod mc procBody = do
  client <- connectSession
  body <- callMethod' client mc
  return $ body >>= procBody

callMethodEmit :: MethodCall
  -> ([Variant] -> Maybe a)
  -> ([Variant] -> Signal)
  -> IO (Maybe a)
callMethodEmit mc procBody bodyToSignal = do
  client <- connectSession
  body <- callMethod' client mc
  forM_ body $ emit client . bodyToSignal
  return $ body >>= procBody

addMatchCallback :: MatchRule -> ([Variant] -> IO ()) -> IO SignalHandler
addMatchCallback rule cb = do
  client <- connectSession
  addMatch client rule $ cb . signalBody
