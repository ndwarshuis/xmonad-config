{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Xmobar.Plugins.Bluetooth where

import Control.Concurrent
import Control.Monad

import qualified Data.Map.Lazy as M
import           Data.Maybe

import DBus
import DBus.Client
import DBus.Internal.Types

import Xmobar

newtype Bluetooth = Bluetooth (String, String, String) deriving (Read, Show)

path :: ObjectPath
path = "/org/bluez/hci0"

interface :: InterfaceName
interface = "org.freedesktop.DBus.Properties"

rule :: MatchRule
rule = matchAny
  { matchPath = Just path
  , matchInterface = Just interface
  , matchMember = Just "PropertiesChanged"
  }

callBT :: Client -> IO (Either MethodError MethodReturn)
callBT client =
  call client (methodCall path interface "Get")
    { methodCallDestination = Just "org.bluez", methodCallBody = body }
  where
    body = map toVariant ["org.bluez.Adapter1", "Powered" :: String]

instance Exec Bluetooth where
  alias (Bluetooth _) = "bluetooth"
  start (Bluetooth (text, colorOn, colorOff)) cb = do
    client <- connectSystem
    _ <- addMatch client rule $
      cb . fmtState . lookupState . getProps . signalBody
    reply <- callBT client
    -- TODO handle errors?
    case reply of
      Right ret -> cb $ fmtState $ fromVariant =<< fromVariant
        =<< listToMaybe (methodReturnBody ret)
      Left _    -> return ()
    forever (threadDelay 5000000)
    where
      -- Assume that the data in the PropertiesChanged signal has the form
      -- [something, Map, something] where the Map in the middle has the
      -- "Powered" text key that we care about (among other things that change
      -- when the bluetooth interface is powered on)
      getProps = \case
        [_, Variant (ValueMap TypeString TypeVariant m), _] -> Just m
        _                                                   -> Nothing
      lookupState m = fromVariant =<< fromValue
        =<< M.lookup (AtomText "Powered") =<< m
      fmtState = \case
        Just s -> wrapColor text $ if s then colorOn else colorOff
        Nothing -> "N/A"
      wrapColor s c = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"
