{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- | High-level interface for managing XMonad's DBus

module XMonad.Internal.DBus.Control
  ( Client
  , startXMonadService
  , stopXMonadService
  , pathExists
  , xmonadBus
  , DBusXMonad(..)
  ) where

import           Data.Either

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Brightness.Common
-- import           XMonad.Internal.DBus.Brightness.IntelBacklight
import           XMonad.Internal.DBus.Common
import           XMonad.Internal.DBus.Screensaver
-- import           XMonad.Internal.Dependency

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

data DBusXMonad = DBusXMonad
  { dxClient             :: Client
  , dxIntelBacklightCtrl :: BrightnessControls
  -- , dxClevoBacklightCtrl :: MaybeExe BrightnessControls
  , dxScreensaverCtrl    :: SSControls
  }

-- blankControls :: BrightnessControls
-- blankControls = BrightnessControls
--   { bctlMax = BlankFeature
--   , bctlMin = BlankFeature
--   , bctlInc = BlankFeature
--   , bctlDec = BlankFeature
--   }

-- blankSSToggle :: SSControls
-- blankSSToggle = SSControls { ssToggle = BlankFeature }

-- xmonadService :: Feature (IO Client)
-- xmonadService = Feature
--   { ftrAction = undefined
--   , ftr


startXMonadService :: IO Client
startXMonadService = do
  client <- connectSession
  res <- requestName client xmonadBus []
  case res of
    NamePrimaryOwner -> return ()
    _                -> putStrLn $ "error when requesting '" ++ formatBusName xmonadBus ++ "'"
  -- TODO if the client is not released on shutdown the owner will be
  -- different
  -- (i, s) <- if requestResult /= NamePrimaryOwner then do
  --   putStrLn "Another service owns \"org.xmonad\""
  --   return (blankControls, blankSSToggle)
  --   else do
  --   putStrLn "Started xmonad dbus client"
  --   bc <- exportIntelBacklight client
  --   sc <- exportScreensaver client
  --   return (bc, sc)
  return client
  -- return $ DBusXMonad
  --   { dxClient = client
  --   , dxIntelBacklightCtrl = i
  --   -- , dxClevoBacklightCtrl = c
  --   , dxScreensaverCtrl = s
  --   }

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
