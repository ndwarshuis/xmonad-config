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
import           XMonad.Internal.Dependency
import           Xmobar.Plugins.Common

newtype Screensaver = Screensaver (String, Colors) deriving (Read, Show)

ssAlias :: String
ssAlias = "screensaver"

instance Exec Screensaver where
  alias (Screensaver _) = ssAlias
  start (Screensaver (text, colors)) cb = do
    withDBusClientConnection cb $ \c -> do
      matchSignal display c
      display =<< callQuery (toClient c)
    where
      display = displayMaybe cb $ return . (\s -> colorText colors s text)

