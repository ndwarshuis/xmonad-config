--------------------------------------------------------------------------------
-- | Screensaver plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.Screensaver
  ( Screensaver(..)
  , ssAlias
  ) where

import           Xmobar

import           XMonad.Internal.DBus.Screensaver
import           Xmobar.Plugins.Common

newtype Screensaver = Screensaver (String, String, String) deriving (Read, Show)

ssAlias :: String
ssAlias = "screensaver"

instance Exec Screensaver where
  alias (Screensaver _) = ssAlias
  start (Screensaver (text, colorOn, colorOff)) cb = do
    withDBusClientConnection False cb $ \c -> do
      matchSignal display c
      display =<< callQuery c
    where
      display = displayMaybe cb $ return . chooseColor text colorOn colorOff

