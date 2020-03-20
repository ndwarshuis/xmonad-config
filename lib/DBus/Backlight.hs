{-# LANGUAGE OverloadedStrings #-}

module DBus.Backlight where

-- import Control.Monad

import Data.Char

import Data.Int (Int32)

-- use strict IO here, the data in these files is literally 1-10 bytes
import Data.Text (pack, unpack)
import Data.Text.IO as T (readFile, writeFile)

import DBus
import DBus.Client

brightnessDir :: FilePath
brightnessDir = "/sys/class/backlight/intel_backlight/"

maxFile :: String
maxFile = brightnessDir ++ "max_brightness"

curFile :: String
curFile = brightnessDir ++ "brightness"

steps :: Int
steps = 15

readFileInt :: FilePath -> IO Int
readFileInt file = do
  contents <- T.readFile file
  return $ read $ takeWhile isDigit $ unpack contents

getMaxValue :: IO Int
getMaxValue = readFileInt maxFile

getCurValue :: IO Int
getCurValue = readFileInt curFile

getStepSize :: IO Int
getStepSize = getMaxValue >>= (\x -> return $ x `div` steps)

setCurValue :: Int -> IO ()
setCurValue = T.writeFile curFile . pack . show

truncateValue :: Int -> Int -> Int
truncateValue maxval = min maxval . max 1

changeBrightness :: Int -> Int -> IO ()
changeBrightness maxval delta = getCurValue
  >>= setCurValue . truncateValue maxval . (+ delta)

setBrightness :: Int -> IO ()
setBrightness = setCurValue

exportBrightness :: Client -> IO ()
exportBrightness client = do
  maxval <- getMaxValue
  stepsize <- getStepSize
  export client "/brightness" defaultInterface
    { interfaceName = "org.xmonad.Brightness"
    , interfaceMethods =
      [ autoMethod "MaxBrightness" (setBrightness maxval)
      , autoMethod "MinBrightness" (setBrightness 1)
      , autoMethod "IncBrightness" (changeBrightness maxval stepsize)
      , autoMethod "DecBrightness" (changeBrightness maxval (-stepsize))
      ]
    }

brPath :: ObjectPath
brPath = "/brightness"

brInterface :: InterfaceName
brInterface = "org.xmonad.Brightness"

brSignal :: Signal
brSignal = (signal brPath brInterface "CurrentBrightness")
  -- { signalDestination = Just "org.xmonad" }

brMatcher :: MatchRule
brMatcher = matchAny
  {
    -- matchSender = Just "org.xmonad"
  -- , matchDestination = Just "org.xmonad"
   -- matchPath = Just brPath
  -- , matchInterface = Just brInterface
   matchMember = Just "CurrentBrightness"
  }

callMaxBrightness :: IO ()
callMaxBrightness = do
  client <- connectSession
  _ <- call client (methodCall "/brightness" "org.xmonad.Brightness" "MaxBrightness")
    { methodCallDestination = Just "org.xmonad" }
  emit client =<< wrapSig <$> getCurValue
  -- print reply
  where
    wrapSig i = brSignal
      { signalBody = [toVariant (fromIntegral i :: Int32)] }

callMinBrightness :: IO ()
callMinBrightness = do
  client <- connectSession
  _ <- call client (methodCall "/brightness" "org.xmonad.Brightness" "MinBrightness")
    { methodCallDestination = Just "org.xmonad" }
  emit client =<< wrapSig <$> getCurValue
  -- print reply
  where
    wrapSig i = brSignal
      { signalBody = [toVariant (fromIntegral i :: Int32)] }
