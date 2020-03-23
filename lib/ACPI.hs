{-# LANGUAGE OverloadedStrings #-}

module ACPI
  ( ACPIEvent(..)
  , acpiMagic
  , isDischarging
  , runPowermon
  ) where

import SendXMsg

import Control.Exception
import Control.Monad

import Data.ByteString       hiding (readFile)
import Data.ByteString.Char8 as C hiding (readFile)
import Data.Connection

import System.IO.Streams.Internal   as S (read)
import System.IO.Streams.UnixSocket

data ACPIEvent = Power
    | Sleep
    | LidClose
    deriving (Eq)

instance Enum ACPIEvent where
  toEnum 0 = Power
  toEnum 1 = Sleep
  toEnum 2 = LidClose
  toEnum _ = errorWithoutStackTrace "ACPI.Enum.ACPIEvent.toEnum"

  fromEnum Power    = 0
  fromEnum Sleep    = 1
  fromEnum LidClose = 2

sendACPIEvent :: ACPIEvent -> IO ()
sendACPIEvent = sendXMsg acpiMagic . show . fromEnum

parseLine :: ByteString -> Maybe ACPIEvent
parseLine line =
  case splitLine line of
    (_:"PBTN":_)        -> Just Power
    (_:"PWRF":_)        -> Just Power
    (_:"SLPB":_)        -> Just Sleep
    (_:"SBTN":_)        -> Just Sleep
    (_:"LID":"close":_) -> Just LidClose
    _                   -> Nothing
  where
    splitLine = C.words . C.reverse . C.dropWhile (== '\n') . C.reverse

isDischarging :: IO (Maybe Bool)
isDischarging = do
  status <- try $ readFile "/sys/class/power_supply/BAT0/status"
    :: IO (Either IOException String)
  case status of
    Left _  -> return Nothing
    Right s -> return $ Just (s == "Discharging")

-- TODO use a data type that enforces strings of max length 5
acpiMagic :: String
acpiMagic = "%acpi"

runPowermon :: IO ()
runPowermon = do
  -- TODO barf when the socket doesn't exist
  Connection { source = s } <- connect "/var/run/acpid.socket"
  forever $ readStream s
  where
    readStream s = do
      out <- S.read s
      mapM_ sendACPIEvent $ parseLine =<< out
