
module Xmobar.Plugins.Common
  ( chooseColor
  , startListener
  )
  where

import           DBus
import           DBus.Client

import           Data.Maybe

import           XMonad.Hooks.DynamicLog     (xmobarColor)
import           XMonad.Internal.DBus.Common

startListener :: IsVariant a => MatchRule -> (Client -> IO [Variant])
  -> ([Variant] -> SignalMatch a) -> (a -> String) -> (String -> IO ()) -> IO ()
startListener rule getProp fromSignal toColor cb = do
  withDBusClientConnection_ True $ \c -> do
    reply <- getProp c
    procMatch $ maybe Failure Match $ fromVariant =<< listToMaybe reply
    addMatchCallback rule (procMatch . fromSignal) c
  where
    procMatch (Match t) = cb $ toColor t
    procMatch Failure   = cb "N/A"
    procMatch NoMatch   = return ()

chooseColor :: String -> String -> String -> Bool -> String
chooseColor text colorOn colorOff state =
  xmobarColor (if state then colorOn else colorOff) "" text
