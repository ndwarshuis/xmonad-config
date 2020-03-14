{-# LANGUAGE OverloadedStrings #-}

import SendXMsg
import ACPI

import Control.Monad

import Data.ByteString
import Data.ByteString.Char8 as C
import Data.Connection

import System.IO.Streams.Internal as S (read)
import System.IO.Streams.UnixSocket

splitLine :: ByteString -> [ByteString]
splitLine = C.words . C.reverse . C.dropWhile (== '\n') . C.reverse

parseLine :: ByteString -> Maybe ACPIEvent
parseLine line = 
  -- TODO what if we don't have a list this long (we crash)
  case (line' !! 1, line' !! 2) of
    ("PBTN", _) -> Just Power
    ("PWRF", _) -> Just Power
    ("SLPB", _) -> Just Sleep
    ("SBTN", _) -> Just Sleep
    ("LID", "close") -> Just LidClose
    _ -> Nothing
  where
    line' = splitLine line

sendACPIEvent :: ACPIEvent -> IO ()
sendACPIEvent = sendXMsg acpiMagic . show

main :: IO ()
main = do
  -- TODO barf when the socket doesn't exist
  Connection { source = s } <- connect "/var/run/acpid.socket"
  forever $ readStream s
  where
    readStream s = do
      out <- (>>= parseLine) <$> S.read s
      forM_ out sendACPIEvent
