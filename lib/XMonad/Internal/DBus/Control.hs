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
import           XMonad.Internal.Shell

startXMonadService :: IO (Client, Maybe BacklightControls, MaybeExe SSControls)
startXMonadService = do
  client <- connectSession
  requestResult <- requestName client "org.xmonad" []
  -- TODO if the client is not released on shutdown the owner will be
  -- different
  if requestResult /= NamePrimaryOwner then do
    putStrLn "Another service owns \"org.xmonad\""
    return (client, Nothing, Ignore)
  else do
    putStrLn "Started xmonad dbus client"
    bc <- exportIntelBacklight client
    sc <- exportScreensaver client
    return (client, bc, sc)

stopXMonadService :: Client -> IO ()
stopXMonadService client = do
  _ <- releaseName client "org.xmonad"
  disconnect client
  return ()

