
module Xmobar.Plugins.Common
  ( chooseColor
  , startListener
  , procSignalMatch
  , na
  , fromSingletonVariant
  , withDBusClientConnection
  , Callback
  , displayMaybe
  , displayMaybe'
  )
  where

import           Control.Monad

import           DBus
import           DBus.Client
import           DBus.Internal

import           XMonad.Hooks.DynamicLog (xmobarColor)

type Callback = String -> IO ()

startListener :: IsVariant a => MatchRule -> (Client -> IO [Variant])
  -> ([Variant] -> SignalMatch a) -> (a -> IO String) -> Callback
  -> Client -> IO ()
startListener rule getProp fromSignal toColor cb client = do
  reply <- getProp client
  displayMaybe cb toColor $ fromSingletonVariant reply
  void $ addMatchCallback rule (procMatch . fromSignal) client
  where
    procMatch = procSignalMatch cb toColor

procSignalMatch :: Callback -> (a -> IO String) -> SignalMatch a -> IO ()
procSignalMatch cb f = withSignalMatch (displayMaybe cb f)

chooseColor :: String -> String -> String -> Bool -> String
chooseColor text colorOn colorOff state =
  xmobarColor (if state then colorOn else colorOff) "" text

na :: String
na = "N/A"

displayMaybe :: Callback -> (a -> IO String) -> Maybe a -> IO ()
displayMaybe cb f = cb <=< maybe (return na) f

displayMaybe' :: Callback -> (a -> IO ()) -> Maybe a -> IO ()
displayMaybe' cb = maybe (cb na)

withDBusClientConnection :: Bool -> Callback -> (Client -> IO ()) -> IO ()
withDBusClientConnection sys cb f = displayMaybe' cb f =<< getDBusClient sys
