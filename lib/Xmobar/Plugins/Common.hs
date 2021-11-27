
module Xmobar.Plugins.Common
  ( chooseColor
  , startListener
  , procSignalMatch
  , na
  , fromSingletonVariant
  , withDBusClientConnection
  , Callback
  )
  where

import           Control.Monad

import           DBus
import           DBus.Client

import           Data.Maybe

import           XMonad.Hooks.DynamicLog     (xmobarColor)
import           XMonad.Internal.DBus.Common

type Callback = String -> IO ()

startListener :: IsVariant a => MatchRule -> (Client -> IO [Variant])
  -> ([Variant] -> SignalMatch a) -> (a -> IO String) -> Callback
  -> Client -> IO ()
startListener rule getProp fromSignal toColor cb client = do
  reply <- getProp client
  procMatch $ maybe Failure Match $ fromVariant =<< listToMaybe reply
  void $ addMatchCallback rule (procMatch . fromSignal) client
  where
    procMatch = procSignalMatch cb toColor

procSignalMatch :: (String -> IO ()) -> (a -> IO String) -> SignalMatch a -> IO ()
procSignalMatch callback formatter (Match x) = callback =<< formatter x
procSignalMatch callback _ Failure           = callback na
procSignalMatch _ _ NoMatch                  = return ()

chooseColor :: String -> String -> String -> Bool -> String
chooseColor text colorOn colorOff state =
  xmobarColor (if state then colorOn else colorOff) "" text

na :: String
na = "N/A"

fromSingletonVariant :: IsVariant a => [Variant] -> Maybe a
fromSingletonVariant = fromVariant <=< listToMaybe

withDBusClientConnection :: Bool -> Callback -> (Client -> IO ()) -> IO ()
withDBusClientConnection sys cb f = maybe (cb na) f =<< getDBusClient sys
