{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Concurrent module to handle events from acpid

module XMonad.Internal.Concurrent.ACPIEvent
  ( runPowermon
  , runHandleACPI
  ) where

import           Control.Exception
import           Control.Monad

import           Data.ByteString                          hiding (readFile)
import           Data.ByteString.Char8                    as C hiding (readFile)
import           Data.Connection

import           Text.Read                                (readMaybe)

import           System.IO.Streams                        as S (read)
import           System.IO.Streams.UnixSocket

import           XMonad.Core
import           XMonad.Internal.Command.Power
import           XMonad.Internal.Concurrent.ClientMessage
import           XMonad.Internal.Dependency
import           XMonad.Internal.Shell
import           XMonad.Internal.Theme
    ( FontBuilder
    , defFontFamily
    )

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

socketDep :: IOTree_
socketDep = Only_ $ pathR acpiPath

-- | Handle ClientMessage event containing and ACPI event (to be used in
-- Xmonad's event hook)
handleACPI :: FontBuilder -> X () -> String -> X ()
handleACPI fb lock tag = do
  let acpiTag = toEnum <$> readMaybe tag :: Maybe ACPIEvent
  forM_ acpiTag $ \case
    Power -> powerPrompt lock fb
    Sleep -> suspendPrompt fb
    LidClose -> do
      status <- io isDischarging
      -- only run suspend if battery exists and is discharging
      forM_ status $ flip when runSuspend
      lock

--------------------------------------------------------------------------------
-- | Exported API

-- | Spawn a new thread that will listen for ACPI events on the acpid socket
-- and send ClientMessage events when it receives them
runPowermon :: SometimesIO
runPowermon = sometimesIO_ "ACPI event monitor" "acpid" socketDep listenACPI

runHandleACPI :: Always (String -> X ())
runHandleACPI = Always "ACPI event handler" $ Option sf fallback
  where
    sf = Subfeature withLock "acpid prompt" Error
    withLock = IORoot (uncurry handleACPI)
      $ And12 (,) (fontTreeAlt defFontFamily) $ Only
      $ IOSometimes runScreenLock id
    fallback = Always_ $ FallbackAlone $ const skip
