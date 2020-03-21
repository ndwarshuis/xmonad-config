{-# LANGUAGE OverloadedStrings #-}

module DBus.Common where

import DBus.IntelBacklight
import DBus.Screensaver

import DBus.Client

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

