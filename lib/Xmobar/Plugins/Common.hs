
module Xmobar.Plugins.Common
  ( chooseColor
  , startListener
  , na
  )
  where

import           DBus
import           DBus.Client

import           Data.Maybe

import           XMonad.Hooks.DynamicLog     (xmobarColor)
import           XMonad.Internal.DBus.Common

startListener :: IsVariant a => MatchRule -> (Client -> IO [Variant])
  -> ([Variant] -> SignalMatch a) -> (a -> String) -> (String -> IO ())
  -> Client -> IO ()
startListener rule getProp fromSignal toColor cb client = do
  reply <- getProp client
  procMatch $ maybe Failure Match $ fromVariant =<< listToMaybe reply
  addMatchCallback rule (procMatch . fromSignal) client
  where
    procMatch (Match t) = cb $ toColor t
    procMatch Failure   = cb na
    procMatch NoMatch   = return ()

chooseColor :: String -> String -> String -> Bool -> String
chooseColor text colorOn colorOff state =
  xmobarColor (if state then colorOn else colorOff) "" text

na :: String
na = "N/A"
