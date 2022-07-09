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

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Common
import           XMonad.Internal.DBus.Screensaver
import           XMonad.Internal.Dependency

-- | Current connections to the DBus (session and system buses)
data DBusState = DBusState
    { dbSesClient :: Maybe SesClient
    , dbSysClient :: Maybe SysClient
    }

-- | Connect to the DBus
connectDBus :: IO DBusState
connectDBus = do
  ses <- getDBusClient
  sys <- getDBusClient
  return DBusState { dbSesClient = ses, dbSysClient = sys }

-- TODO why is this only the session client?
-- | Disconnect from the DBus
disconnectDBus :: DBusState -> IO ()
disconnectDBus db = forM_ (toClient <$> dbSysClient db) disconnect

-- | Connect to the DBus and request the XMonad name
connectDBusX :: IO DBusState
connectDBusX = do
  db <- connectDBus
  forM_ (toClient <$> dbSesClient db) requestXMonadName
  return db

-- | Disconnect from DBus and release the XMonad name
disconnectDBusX :: DBusState -> IO ()
disconnectDBusX db = do
  forM_ (toClient <$> dbSesClient db) releaseXMonadName
  disconnectDBus db

-- | All exporter features to be assigned to the DBus
dbusExporters :: [Maybe SesClient -> SometimesIO]
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
