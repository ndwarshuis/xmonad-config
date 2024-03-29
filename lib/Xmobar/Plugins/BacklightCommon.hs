--------------------------------------------------------------------------------
-- | Common backlight plugin bits
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.BacklightCommon (startBacklight) where

import           Data.Internal.DBus

import           Xmobar.Plugins.Common

startBacklight :: RealFrac a => ((Maybe a -> IO ()) -> SesClient -> IO ())
  -> (SesClient -> IO (Maybe a)) -> String -> Callback -> IO ()
startBacklight matchSignal callGetBrightness icon cb = do
    withDBusClientConnection cb $ \c -> do
      matchSignal display c
      display =<< callGetBrightness c
    where
      formatBrightness b = return $ icon ++ show (round b :: Integer) ++ "%"
      display = displayMaybe cb formatBrightness
