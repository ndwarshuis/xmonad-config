{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Control
  ( Client
  , startXMonadService
  , getDBusClient
  , stopXMonadService
  , pathExists
  , disconnect
  ) where

import           Control.Exception
import           Control.Monad                                  (forM_, void)

import           Data.Either

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Brightness.ClevoKeyboard
import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Common
import           XMonad.Internal.DBus.Screensaver
import           XMonad.Internal.Dependency

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

startXMonadService :: IO (Maybe Client)
startXMonadService = do
  client <- getDBusClient False
  forM_ client requestXMonadName
  mapM_ (\f -> executeFeature_ $ f client) exporters
  return client
  where
    exporters = [exportScreensaver, exportIntelBacklight, exportClevoKeyboard]

stopXMonadService :: Client -> IO ()
stopXMonadService client = do
  void $ releaseName client xmonadBusName
  disconnect client

getDBusClient :: Bool -> IO (Maybe Client)
getDBusClient sys = do
  res <- try $ if sys then connectSystem else connectSession
  case res of
    Left e  -> putStrLn (clientErrorMessage e) >> return Nothing
    Right c -> return $ Just c

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

pathExists :: Bool -> BusName -> ObjectPath -> IO Bool
pathExists sysbus n p = do
  client <- if sysbus then connectSystem else connectSession
  r <- call client (methodCall p introspectInterface introspectMethod)
       { methodCallDestination = Just n }
  disconnect client
  return $ isRight r
