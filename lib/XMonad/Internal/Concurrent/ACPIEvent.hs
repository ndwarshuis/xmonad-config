{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Concurrent module to handle events from acpid

module XMonad.Internal.Concurrent.ACPIEvent
  ( runPowermon
  , handleACPI
  ) where

import           Control.Exception
import           Control.Monad

import           Data.ByteString                          hiding (readFile)
import           Data.ByteString.Char8                    as C hiding (readFile)
import           Data.Connection

import           Text.Read                                (readMaybe)

-- import           System.Directory                         (doesPathExist)
import           System.IO.Streams                        as S (read)
import           System.IO.Streams.UnixSocket

import           XMonad.Core
import           XMonad.Internal.Command.Power
import           XMonad.Internal.Concurrent.ClientMessage
import           XMonad.Internal.Dependency

--------------------------------------------------------------------------------
-- | Data structure to hold the ACPI events I care about
--
-- Enumerate so these can be converted to strings and back when sent in a
-- ClientMessage event to X

data ACPIEvent = Power
    | Sleep
    | LidClose
    deriving (Eq)

instance Enum ACPIEvent where
  toEnum 0 = Power
  toEnum 1 = Sleep
  toEnum 2 = LidClose
  toEnum _ = errorWithoutStackTrace "ACPI.Enum.ACPIEvent.toEnum: bad argument"

  fromEnum Power    = 0
  fromEnum Sleep    = 1
  fromEnum LidClose = 2

--------------------------------------------------------------------------------
-- | Internal functions

-- | Convert a string to an ACPI event (this string is assumed to come from
-- the acpid socket)
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

-- | Send an ACPIEvent to the X server as a ClientMessage
sendACPIEvent :: ACPIEvent -> IO ()
sendACPIEvent = sendXMsg ACPI . show . fromEnum

isDischarging :: IO (Maybe Bool)
isDischarging = do
  status <- try $ readFile "/sys/class/power_supply/BAT0/status"
    :: IO (Either IOException String)
  case status of
    Left _  -> return Nothing
    Right s -> return $ Just (s == "Discharging")

listenACPI :: IO ()
listenACPI = do
  Connection { source = s } <- connect acpiPath
  forever $ readStream s
  where
    readStream s = do
      out <- S.read s
      mapM_ sendACPIEvent $ parseLine =<< out

acpiPath :: FilePath
acpiPath = "/var/run/acpid.socket"

--------------------------------------------------------------------------------
-- | Exported API

-- | Spawn a new thread that will listen for ACPI events on the acpid socket
-- and send ClientMessage events when it receives them
runPowermon :: SometimesIO
runPowermon = sometimesIO "ACPI event monitor" (Only $ pathR acpiPath) listenACPI

-- | Handle ClientMessage event containing and ACPI event (to be used in
-- Xmonad's event hook)
handleACPI :: X () -> String -> X ()
handleACPI lock tag = do
  let acpiTag = toEnum <$> readMaybe tag :: Maybe ACPIEvent
  forM_ acpiTag $ \case
    Power -> runPowerPrompt lock
    Sleep -> runSuspendPrompt
    LidClose -> do
      status <- io isDischarging
      -- only run suspend if battery exists and is discharging
      forM_ status $ flip when runSuspend
      lock

