{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Control
  ( Client
  , startXMonadService
  , getDBusClient
  , withDBusClient
  , withDBusClient_
  , stopXMonadService
  , disconnect
  , dbusExporters
  ) where

import           Control.Monad                                  (forM_, void)

import           DBus
import           DBus.Client
import           DBus.Internal

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Common
import           XMonad.Internal.DBus.Screensaver
import           XMonad.Internal.Dependency

startXMonadService :: IO (Maybe Client)
startXMonadService = do
  client <- getDBusClient False
  forM_ client requestXMonadName
  mapM_ (\f -> executeSometimes_ $ f client) dbusExporters
  return client

stopXMonadService :: Client -> IO ()
stopXMonadService client = do
  void $ releaseName client xmonadBusName
  disconnect client

requestXMonadName :: Client -> IO ()
requestXMonadName client = do
  res <- requestName client xmonadBusName []
  -- TODO if the client is not released on shutdown the owner will be different
  let msg | res == NamePrimaryOwner = Nothing
          | res == NameAlreadyOwner = Just $ "this process already owns " ++ xn
          | res == NameInQueue
            || res == NameExists = Just $ "another process owns " ++ xn
          | otherwise = Just $ "unknown error when requesting " ++ xn
  forM_ msg putStrLn
  where
    xn = "'" ++ formatBusName xmonadBusName ++ "'"

dbusExporters :: [Maybe Client -> SometimesIO]
dbusExporters = [exportScreensaver, exportIntelBacklight, exportClevoKeyboard]
