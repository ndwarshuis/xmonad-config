--------------------------------------------------------------------------------
-- | Clevo Keyboard plugin
--
-- Use the custom DBus interface exported by the XMonad process so I can react
-- to signals spawned by commands

module Xmobar.Plugins.ClevoKeyboard
  ( ClevoKeyboard(..)
  , ckAlias
  ) where

import           Data.Char
import           Data.Text               (unpack)
import           Data.Text.IO            as T (readFile)

import           Xmobar

import           XMonad.Hooks.DynamicLog (xmobarColor)

-- import           XMonad.Internal.DBus.IntelBacklight

data ClevoKeyboard = ClevoKeyboard (String, String, String) Int
  deriving (Read, Show)

ckAlias :: String
ckAlias = "clevokeyboard"

brightnessFile :: FilePath
brightnessFile = "/sys/devices/platform/tuxedo_keyboard/brightness"

stateFile :: FilePath
stateFile = "/sys/devices/platform/tuxedo_keyboard/state"

readBrightness :: FilePath -> IO Integer
readBrightness file = read . takeWhile isDigit . unpack <$> T.readFile file

readState :: FilePath -> IO Bool
readState file = (==1) <$> readBrightness file

instance Exec ClevoKeyboard where
  alias (ClevoKeyboard _ _) = ckAlias
  rate (ClevoKeyboard _ r) = r
  run (ClevoKeyboard (icon, colorOn, colorOff) _) = do
    b <- readBrightness brightnessFile
    s <- readState stateFile
    return $ formatBrightness s (fromIntegral b :: Double)
    where
      formatBrightness s b =
        let iconColor = if s then colorOn else colorOff
            n = show (round $ b / 255 * 100 :: Integer) ++ "%"
        in xmobarColor iconColor "" icon ++ n
