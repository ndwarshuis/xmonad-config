{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | DBus module for Intel Backlight control

module XMonad.Internal.DBus.IntelBacklight
  ( callDecBrightness
  , callGetBrightness
  , callIncBrightness
  , callMaxBrightness
  , callMinBrightness
  , exportIntelBacklight
  , matchSignal
  , hasBacklight
  , BacklightControls(..)
  ) where

import           Control.Monad               (void)

import           Data.Char
import           Data.Either
import           Data.Int                    (Int16, Int32)
import           Data.Text                   (pack, unpack)
import           Data.Text.IO                as T (readFile, writeFile)

import           DBus
import           DBus.Client

import           System.Directory
import           System.FilePath.Posix
import           System.IO.Error

import           XMonad.Internal.DBus.Common

--------------------------------------------------------------------------------
-- | Low level sysfs functions
--
-- Distinguish between "raw" brightness "normalized" brightness with two type
-- synonyms. The former is the value read directly in sysfs and generally goes
-- from 1 (min brightness) to some multiple 1000's number (note that raw values
-- of 0 turn the monitor off). The latter is the raw brightness scaled from 0 to
-- 10000 (which can easily be converted to a percent).

-- use strict IO here, the data in these files is literally 1-10 bytes

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

maxFile :: FilePath
maxFile = backlightDir </> "max_brightness"

curFile :: FilePath
curFile = backlightDir </> "brightness"

readFileInt :: FilePath -> IO RawBrightness
readFileInt file = read . takeWhile isDigit . unpack <$> T.readFile file

getMaxRawBrightness :: IO RawBrightness
getMaxRawBrightness = readFileInt maxFile

getRawBrightness :: IO RawBrightness
getRawBrightness = readFileInt curFile

setRawBrightness :: RawBrightness -> IO ()
setRawBrightness = T.writeFile curFile . pack . show

-- TODO this has rounding errors that make the steps uneven
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
  . (+ delta) =<< getBrightness maxRaw

setBrightness :: RawBrightness -> Brightness -> IO Brightness
setBrightness maxRaw newNorm = do
  let newNorm' = truncateNorm newNorm
  setRawBrightness $ normToRaw maxRaw newNorm'
  return newNorm'

--------------------------------------------------------------------------------
-- | Access checks

-- | determine if backlight is accessible/present
-- Right True -> backlight accessible and present
-- Right False -> backlight not present
-- Left x -> backlight present but could not access (x explaining why)
hasBacklight' :: IO (Either String Bool)
hasBacklight' = do
  mx <- doesFileExist maxFile
  cx <- doesFileExist curFile
  if not $ mx || cx
    then return $ Right False
    else do
    mp <- tryIOError $ readable <$> getPermissions maxFile
    cp <- tryIOError $ (\p -> writable p && readable p) <$> getPermissions curFile
    return $ case (mp, cp) of
      (Right True, Right True) -> Right True
      (Right _, Right _) -> Left "Insufficient permissions for backlight files"
      _                  -> Left "Could not determine backlight file permissions"

msg :: Either String Bool -> IO ()
msg (Right True)  = return ()
msg (Right False) = print ("No backlight detected. Controls disabled" :: String)
msg (Left m)      = print ("WARNING: " ++ m)

hasBacklightMsg :: IO Bool
hasBacklightMsg = do
  b <- hasBacklight'
  msg b
  return $ fromRight False b

hasBacklight :: IO Bool
hasBacklight = fromRight False <$> hasBacklight'

--------------------------------------------------------------------------------
-- | DBus interface
--
-- Define four methods to increase, decrease, maximize, or minimize the
-- brightness. These methods will all return the current brightness as a 32-bit
-- integer and emit a signal with the same brightness value. Additionally, there
-- is one method to get the current brightness.

path :: ObjectPath
path = "/intelbacklight"

interface :: InterfaceName
interface = "org.xmonad.Brightness"

memCurrentBrightness :: MemberName
memCurrentBrightness = "CurrentBrightness"

memGetBrightness :: MemberName
memGetBrightness = "GetBrightness"

memMaxBrightness :: MemberName
memMaxBrightness = "MaxBrightness"

memMinBrightness :: MemberName
memMinBrightness = "MinBrightness"

memIncBrightness :: MemberName
memIncBrightness = "IncBrightness"

memDecBrightness :: MemberName
memDecBrightness = "DecBrightness"

brSignal :: Signal
brSignal = signal path interface memCurrentBrightness
  -- { signalDestination = Just "org.xmonad" }

brMatcher :: MatchRule
brMatcher = matchAny
  { matchPath = Just path
  , matchInterface = Just interface
  , matchMember = Just memCurrentBrightness
  }

callBacklight :: MemberName -> IO ()
callBacklight method = void $ callMethod $ methodCall path interface method

bodyGetBrightness :: [Variant] -> Maybe Brightness
bodyGetBrightness [b] = fromVariant b :: Maybe Brightness
bodyGetBrightness _   = Nothing

--------------------------------------------------------------------------------
-- | Exported haskell API

data BacklightControls = BacklightControls
  { backlightMax  :: IO ()
  , backlightMin  :: IO ()
  , backlightUp   :: IO ()
  , backlightDown :: IO ()
  }

exportIntelBacklight :: Client -> IO (Maybe BacklightControls)
exportIntelBacklight client = do
  b <- hasBacklightMsg
  if b then Just <$> exportIntelBacklight' client else return Nothing

exportIntelBacklight' :: Client -> IO BacklightControls
exportIntelBacklight' client = do
  maxval <- getMaxRawBrightness -- assume the max value will never change
  let stepsize = maxBrightness `div` steps
  let emit' = emitBrightness client
  export client path defaultInterface
    { interfaceName = interface
    , interfaceMethods =
      [ autoMethod memMaxBrightness $ emit' =<< setBrightness maxval maxBrightness
      , autoMethod memMinBrightness $ emit' =<< setBrightness maxval 0
      , autoMethod memIncBrightness $ emit' =<< changeBrightness maxval stepsize
      , autoMethod memDecBrightness $ emit' =<< changeBrightness maxval (-stepsize)
      , autoMethod memGetBrightness $ getBrightness maxval
      ]
    }
  return $ BacklightControls
    { backlightMax = callMaxBrightness
    , backlightMin = callMinBrightness
    , backlightUp = callIncBrightness
    , backlightDown = callDecBrightness
    }

emitBrightness :: Client -> Brightness -> IO ()
emitBrightness client cur = emit client $ brSignal { signalBody = [toVariant cur] }

callMaxBrightness :: IO ()
callMaxBrightness = callBacklight memMaxBrightness

callMinBrightness :: IO ()
callMinBrightness = callBacklight memMinBrightness

callIncBrightness :: IO ()
callIncBrightness = callBacklight memIncBrightness

callDecBrightness :: IO ()
callDecBrightness = callBacklight memDecBrightness

callGetBrightness :: IO (Maybe Brightness)
callGetBrightness = do
  reply <- callMethod $ methodCall path interface memGetBrightness
  return $ reply >>= bodyGetBrightness

matchSignal :: (Maybe Brightness -> IO ()) -> IO SignalHandler
matchSignal cb = do
  client <- connectSession
  addMatch client brMatcher $ cb . bodyGetBrightness . signalBody
