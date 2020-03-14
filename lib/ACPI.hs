module ACPI (ACPIEvent(..), acpiMagic, isDischarging) where

import Control.Exception
import Text.Read

data ACPIEvent = Power | Sleep | LidClose deriving (Eq)

instance Show ACPIEvent where
  show Power = "power"
  show Sleep = "sleep"
  -- show LidOpen = "olid"
  show LidClose = "clid"

instance Read ACPIEvent where
  readPrec = do
    Ident s <- lexP
    case s of
      -- TODO this isn't DRY
      "power" -> return Power
      "sleep" -> return Sleep
      "clid" -> return LidClose
      _ -> pfail

-- TODO use a data type that enforces strings of max length 5
acpiMagic :: String
acpiMagic = "%acpi"

isDischarging :: IO (Maybe Bool)
isDischarging = do
  status <- try $ readFile "/sys/class/power_supply/BAT0/status"
    :: IO (Either IOException String)
  case status of
    Left e -> do
      print e
      return Nothing
    Right s -> return $ Just (s == "Discharging")
