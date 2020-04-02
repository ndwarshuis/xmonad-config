{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Control
  ( Client
  , startXMonadService
  , stopXMonadService
  ) where

import           DBus.Client

import           XMonad.Internal.DBus.IntelBacklight
import           XMonad.Internal.DBus.Screensaver

startXMonadService :: IO Client
startXMonadService = do
  client <- connectSession
  requestResult <- requestName client "org.xmonad" []
  -- TODO if the client is not released on shutdown the owner will be
  -- different
  if requestResult /= NamePrimaryOwner then
    putStrLn "Another service owns \"org.xmonad\""
  else do
    putStrLn "Started xmonad dbus client"
    exportIntelBacklight client
    exportScreensaver client
  return client

stopXMonadService :: Client -> IO ()
stopXMonadService client = do
  _ <- releaseName client "org.xmonad"
  disconnect client
  return ()

