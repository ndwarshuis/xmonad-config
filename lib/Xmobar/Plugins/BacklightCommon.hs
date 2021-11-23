--------------------------------------------------------------------------------
-- | Common backlight plugin bits
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.BacklightCommon (startBacklight) where

import           Control.Concurrent
import           Control.Monad

import           DBus.Client

import           XMonad.Internal.DBus.Control

startBacklight :: RealFrac a => ((Maybe a -> IO ()) -> Client -> IO ())
  -> (Client -> IO (Maybe a)) -> String -> (String -> IO ()) -> IO ()
startBacklight matchSignal callGetBrightness icon cb = do
    withDBusClient_ False $ \c -> do
      matchSignal (cb . formatBrightness) c
      cb . formatBrightness =<< callGetBrightness c
    forever (threadDelay 5000000)
    where
      formatBrightness = maybe "N/A" $
        \b -> icon ++ show (round b :: Integer) ++ "%"
