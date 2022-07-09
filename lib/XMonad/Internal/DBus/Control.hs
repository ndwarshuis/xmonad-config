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

import           Data.Internal.DBus
import           Data.Internal.Dependency

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Common
import           XMonad.Internal.DBus.Screensaver

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

-- | Disconnect from the DBus
disconnectDBus :: DBusState -> IO ()
disconnectDBus db = disc dbSesClient >> disc dbSysClient
  where
    disc f = maybe (return ()) disconnectDBusClient $ f db

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
dbusExporters :: [Maybe SesClient -> SometimesIO]
dbusExporters = [exportScreensaver, exportIntelBacklight, exportClevoKeyboard]

releaseXMonadName :: SesClient -> IO ()
releaseXMonadName ses = void $ releaseName (toClient ses) xmonadBusName

requestXMonadName :: SesClient -> IO ()
requestXMonadName ses = do
  res <- requestName (toClient ses) xmonadBusName []
  -- TODO if the client is not released on shutdown the owner will be different
  let msg | res == NamePrimaryOwner = Nothing
          | res == NameAlreadyOwner = Just $ "this process already owns " ++ xn
          | res == NameInQueue
            || res == NameExists = Just $ "another process owns " ++ xn
          | otherwise = Just $ "unknown error when requesting " ++ xn
  forM_ msg putStrLn
  where
    xn = "'" ++ formatBusName xmonadBusName ++ "'"
