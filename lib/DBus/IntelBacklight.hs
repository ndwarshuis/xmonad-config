{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | DBus module for Intel Backlight control

module DBus.IntelBacklight
  ( callDecBrightness
  , callGetBrightness
  , callIncBrightness
  , callMaxBrightness
  , callMinBrightness
  , exportIntelBacklight
  , matchSignal
  ) where

import Control.Monad (forM_)

import Data.Char

import Data.Int (Int16, Int32)

-- use strict IO here, the data in these files is literally 1-10 bytes
import Data.Text    (pack, unpack)
import Data.Text.IO as T (readFile, writeFile)

import DBus
import DBus.Client

--------------------------------------------------------------------------------
-- | Low level sysfs functions
--
-- Distinguish between "raw" brightness "normalized" brightness with two type
-- synonyms. The former is the value read directly in sysfs and generally goes
-- from 1 (min brightness) to some multiple 1000's number (note that raw values
-- of 0 turn the monitor off). The latter is the raw brightness scaled from 0 to
-- 10000 (which can easily be converted to a percent).

-- TODO this is hacky but not sure if there is a cleaner way to enforce type
-- checking between these without making two new types and adding Integral
-- instances to both of them
type Brightness = Int16

type RawBrightness = Int32

maxBrightness :: Brightness
maxBrightness = 10000

steps :: Brightness
steps = 16

backlightDir :: FilePath
backlightDir = "/sys/class/backlight/intel_backlight/"

maxFile :: String
maxFile = backlightDir ++ "max_brightness"

curFile :: String
curFile = backlightDir ++ "brightness"

readFileInt :: FilePath -> IO RawBrightness
readFileInt file = read . takeWhile isDigit . unpack <$> T.readFile file

getMaxRawBrightness :: IO RawBrightness
getMaxRawBrightness = readFileInt maxFile

getRawBrightness :: IO RawBrightness
getRawBrightness = readFileInt curFile

setRawBrightness :: RawBrightness -> IO ()
setRawBrightness = T.writeFile curFile . pack . show

rawToNorm :: RawBrightness -> RawBrightness -> Brightness
rawToNorm maxRaw curRaw = fromIntegral
  $ (curRaw - 1) * maxNorm `div` (maxRaw - 1)
  where
    maxNorm = fromIntegral maxBrightness :: Int32

normToRaw :: RawBrightness -> Brightness -> RawBrightness
normToRaw maxRaw curNorm = curNorm' * (maxRaw - 1) `div` maxNorm + 1
  where
    curNorm' = fromIntegral curNorm :: Int32
    maxNorm = fromIntegral maxBrightness :: Int32

truncateNorm :: Brightness -> Brightness
truncateNorm = min maxBrightness . max 0

getBrightness :: RawBrightness -> IO Brightness
getBrightness maxRaw = rawToNorm maxRaw <$> getRawBrightness

changeBrightness :: RawBrightness -> Brightness -> IO Brightness
changeBrightness maxRaw delta = setBrightness maxRaw
  =<< (+ delta) <$> getBrightness maxRaw

setBrightness :: RawBrightness -> Brightness -> IO Brightness
setBrightness maxRaw newNorm = do
  let newNorm' = truncateNorm newNorm
  setRawBrightness $ normToRaw maxRaw newNorm'
  return newNorm'

--------------------------------------------------------------------------------
-- | DBus interface
--
-- Define four methods to increase, decrease, maximize, or minimize the
-- brightness. These methods will all return the current brightness as a 32-bit
-- integer and emit a signal with the same brightness value. Additionally, there
-- is one method to get the current brightness.

brPath :: ObjectPath
brPath = "/intelbacklight"

brInterface :: InterfaceName
brInterface = "org.xmonad.Brightness"

brCurrentBrightness :: MemberName
brCurrentBrightness = "CurrentBrightness"

brGetBrightness :: MemberName
brGetBrightness = "GetBrightness"

brMaxBrightness :: MemberName
brMaxBrightness = "MaxBrightness"

brMinBrightness :: MemberName
brMinBrightness = "MinBrightness"

brIncBrightness :: MemberName
brIncBrightness = "IncBrightness"

brDecBrightness :: MemberName
brDecBrightness = "DecBrightness"

brSignal :: Signal
brSignal = signal brPath brInterface brCurrentBrightness
  -- { signalDestination = Just "org.xmonad" }

brMatcher :: MatchRule
brMatcher = matchAny
  { matchPath = Just brPath
  , matchInterface = Just brInterface
  , matchMember = Just brCurrentBrightness
  }

callBacklight :: Client -> MemberName -> IO (Maybe [Variant])
callBacklight client method = do
  -- TODO this will throw a clienterror if it cannot connect at all
  reply <- call client (methodCall brPath brInterface method)
    { methodCallDestination = Just "org.xmonad" }
  return $ case reply of
    Left _    -> Nothing
    Right ret -> Just $ methodReturnBody ret

callBacklight' :: MemberName -> IO (Maybe Brightness)
callBacklight' method = do
  client <- connectSession
  body <- callBacklight client method
  -- TODO this is a bit convoluted...I return the body in the reply of
  -- the method call and feed that to the signal and then return the
  -- body (the latter is not really necessary since the only things
  -- that read the backlight status either use the signal or call
  -- GetBrightness directly
  forM_ body $ emitBrightness client
  return $ body >>= signalBrightness

emitBrightness :: Client -> [Variant] -> IO ()
emitBrightness client body =
  emit client $ brSignal { signalBody = body }

signalBrightness :: [Variant] -> Maybe Brightness
signalBrightness [b] = fromVariant b :: Maybe Brightness
signalBrightness _   = Nothing

-- | Exported haskell API

exportIntelBacklight :: Client -> IO ()
exportIntelBacklight client = do
  maxval <- getMaxRawBrightness -- assume the max value will never change
  let stepsize = maxBrightness `div` steps
  export client brPath defaultInterface
    { interfaceName = brInterface
    , interfaceMethods =
      [ autoMethod brMaxBrightness (setBrightness maxval maxBrightness)
      , autoMethod brMinBrightness (setBrightness maxval 0)
      , autoMethod brIncBrightness (changeBrightness maxval stepsize)
      , autoMethod brDecBrightness (changeBrightness maxval (-stepsize))
      , autoMethod brGetBrightness (getBrightness maxval)
      ]
    }

callMaxBrightness :: IO (Maybe Brightness)
callMaxBrightness = callBacklight' brMaxBrightness

callMinBrightness :: IO (Maybe Brightness)
callMinBrightness = callBacklight' brMinBrightness

callIncBrightness :: IO (Maybe Brightness)
callIncBrightness = callBacklight' brIncBrightness

callDecBrightness :: IO (Maybe Brightness)
callDecBrightness = callBacklight' brDecBrightness

callGetBrightness :: IO (Maybe Brightness)
callGetBrightness = do
  client <- connectSession
  body <- callBacklight client brGetBrightness
  return $ body >>= signalBrightness

matchSignal :: (Maybe Brightness -> IO ()) -> IO SignalHandler
matchSignal cb = do
  client <- connectSession
  addMatch client brMatcher $ cb . pullBrightness . signalBody
  where
    pullBrightness = \case
      [b] -> fromVariant b :: Maybe Brightness
      _   -> Nothing
