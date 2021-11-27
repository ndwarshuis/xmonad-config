--------------------------------------------------------------------------------
-- | Common backlight plugin bits
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.BacklightCommon (startBacklight) where

import           DBus.Client

import           Xmobar.Plugins.Common

startBacklight :: RealFrac a => ((Maybe a -> IO ()) -> Client -> IO ())
  -> (Client -> IO (Maybe a)) -> String -> (String -> IO ()) -> IO ()
startBacklight matchSignal callGetBrightness icon cb = do
    withDBusClientConnection False cb $ \c -> do
      matchSignal (cb . formatBrightness) c
      cb . formatBrightness =<< callGetBrightness c
    where
      formatBrightness = maybe na $
        \b -> icon ++ show (round b :: Integer) ++ "%"
