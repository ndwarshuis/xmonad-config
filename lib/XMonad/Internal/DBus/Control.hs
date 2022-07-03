{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Control
  ( Client
  , DBusState(..)
  , connectDBus
  , connectDBusX
  , disconnectDBus
  , disconnectDBusX
  , getDBusClient
  , withDBusClient
  , withDBusClient_
  , disconnect
  , dbusExporters
  ) where

import           Control.Monad

import           DBus
import           DBus.Client
import           DBus.Internal

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Common
import           XMonad.Internal.DBus.Screensaver
import           XMonad.Internal.Dependency

-- | Current connections to the DBus (session and system buses)
data DBusState = DBusState
    { dbSesClient :: Maybe Client
    , dbSysClient :: Maybe Client
    }

-- | Connect to the DBus
connectDBus :: IO DBusState
connectDBus = do
  ses <- getDBusClient False
  sys <- getDBusClient True
  return DBusState { dbSesClient = ses, dbSysClient = sys }

-- | Disconnect from the DBus
disconnectDBus :: DBusState -> IO ()
disconnectDBus db = forM_ (dbSysClient db) disconnect

-- | Connect to the DBus and request the XMonad name
connectDBusX :: IO DBusState
connectDBusX = do
  db <- connectDBus
  forM_ (dbSesClient db) requestXMonadName
  return db

-- | Disconnect from DBus and release the XMonad name
disconnectDBusX :: DBusState -> IO ()
disconnectDBusX db = do
  forM_ (dbSesClient db) releaseXMonadName
  disconnectDBus db

-- | All exporter features to be assigned to the DBus
dbusExporters :: [Maybe Client -> SometimesIO]
dbusExporters = [exportScreensaver, exportIntelBacklight, exportClevoKeyboard]

releaseXMonadName :: Client -> IO ()
releaseXMonadName cl = void $ releaseName cl xmonadBusName

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

-- executeExporters :: Maybe Client -> IO ()
-- executeExporters cl = mapM_ (\f -> executeSometimes $ f cl) dbusExporters
