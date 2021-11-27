
module Xmobar.Plugins.Common
  ( colorText
  , startListener
  , procSignalMatch
  , na
  , fromSingletonVariant
  , withDBusClientConnection
  , Callback
  , Colors(..)
  , displayMaybe
  , displayMaybe'
  , xmobarFGColor
  )
  where

import           Control.Monad

import           DBus
import           DBus.Client
import           DBus.Internal

import           XMonad.Hooks.DynamicLog (xmobarColor)

type Callback = String -> IO ()

data Colors = Colors
  { colorsOn  :: String
  , colorsOff :: String
  }
  deriving (Eq, Show, Read)

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

colorText :: Colors -> Bool -> String -> String
colorText Colors { colorsOn = c } True   = xmobarFGColor c
colorText Colors { colorsOff = c } False = xmobarFGColor c

xmobarFGColor :: String -> String -> String
xmobarFGColor c = xmobarColor c ""

na :: String
na = "N/A"

displayMaybe :: Callback -> (a -> IO String) -> Maybe a -> IO ()
displayMaybe cb f = cb <=< maybe (return na) f

displayMaybe' :: Callback -> (a -> IO ()) -> Maybe a -> IO ()
displayMaybe' cb = maybe (cb na)

withDBusClientConnection :: Bool -> Callback -> (Client -> IO ()) -> IO ()
withDBusClientConnection sys cb f = displayMaybe' cb f =<< getDBusClient sys
