{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Control
  ( Client
  , startXMonadService
  , stopXMonadService
  , pathExists
  , xmonadBus
  ) where

import           Data.Either

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Common
import           XMonad.Internal.DBus.IntelBacklight
import           XMonad.Internal.DBus.Screensaver
import           XMonad.Internal.Shell

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

startXMonadService :: IO (Client, Maybe BacklightControls, MaybeExe SSControls)
startXMonadService = do
  client <- connectSession
  requestResult <- requestName client xmonadBus []
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
  _ <- releaseName client xmonadBus
  disconnect client
  return ()

pathExists :: Bool -> BusName -> ObjectPath -> IO Bool
pathExists sysbus n p = do
  client <- if sysbus then connectSystem else connectSession
  r <- call client (methodCall p introspectInterface introspectMethod)
       { methodCallDestination = Just n }
  disconnect client
  return $ isRight r
