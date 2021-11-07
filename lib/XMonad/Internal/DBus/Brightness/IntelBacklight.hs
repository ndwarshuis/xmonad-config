--------------------------------------------------------------------------------
-- | DBus module for Intel Backlight control

module XMonad.Internal.DBus.Brightness.IntelBacklight
  ( callGetBrightnessIB
  , matchSignalIB
  , exportIntelBacklight
  , hasBacklight
  , blPath
  ) where

import           Data.Either
import           Data.Int                               (Int32)

import           DBus
import           DBus.Client

import           System.FilePath.Posix

import           XMonad.Internal.DBus.Brightness.Common
import           XMonad.Internal.IO

--------------------------------------------------------------------------------
-- | Low level sysfs functions
--
type Brightness = Float

type RawBrightness = Int32

steps :: Int
steps = 16

backlightDir :: FilePath
backlightDir = "/sys/class/backlight/intel_backlight/"

maxFile :: FilePath
maxFile = backlightDir </> "max_brightness"

curFile :: FilePath
curFile = backlightDir </> "brightness"

getMaxRawBrightness :: IO RawBrightness
getMaxRawBrightness = readInt maxFile

getBrightness :: RawBrightness -> IO Brightness
getBrightness upper = readPercent upper curFile

minBrightness :: RawBrightness -> IO Brightness
minBrightness upper = writePercentMin upper curFile

maxBrightness :: RawBrightness -> IO Brightness
maxBrightness upper = writePercentMax upper curFile

incBrightness :: RawBrightness -> IO Brightness
incBrightness = incPercent steps curFile

decBrightness :: RawBrightness -> IO Brightness
decBrightness = decPercent steps curFile

--------------------------------------------------------------------------------
-- | Access checks

-- | determine if backlight is accessible/present
-- Right True -> backlight accessible and present
-- Right False -> backlight not present
-- Left x -> backlight present but could not access (x explaining why)
hasBacklight' :: IO (Either String Bool)
hasBacklight' = do
  mx <- isReadable maxFile
  cx <- isWritable curFile
  return $ case (mx, cx) of
    (NotFoundError, NotFoundError) -> Right False
    (PermResult True, PermResult True) -> Right True
    (PermResult _, PermResult _) -> Left "Insufficient permissions for backlight files"
    _ -> Left "Could not determine permissions for backlight files"

msg :: Either String Bool -> IO ()
msg (Right True)  = return ()
msg (Right False) = putStrLn "No backlight detected. Controls disabled"
msg (Left m)      = putStrLn $ "WARNING: " ++ m

hasBacklightMsg :: IO Bool
hasBacklightMsg = do
  b <- hasBacklight'
  msg b
  return $ fromRight False b

hasBacklight :: IO Bool
hasBacklight = fromRight False <$> hasBacklight'

--------------------------------------------------------------------------------
-- | DBus interface

blPath :: ObjectPath
blPath = objectPath_ "/intelbacklight"

interface :: InterfaceName
interface = interfaceName_ "org.xmonad.Brightness"

intelBacklightConfig :: BrightnessConfig RawBrightness Brightness
intelBacklightConfig = BrightnessConfig
  { bcMin = minBrightness
  , bcMax = maxBrightness
  , bcInc = incBrightness
  , bcDec = decBrightness
  , bcGet = getBrightness
  , bcGetMax = getMaxRawBrightness
  , bcPath = blPath
  , bcInterface = interface
  }

--------------------------------------------------------------------------------
-- | Exported haskell API

exportIntelBacklight :: Client -> IO (Maybe BrightnessControls)
exportIntelBacklight client = do
  b <- hasBacklightMsg
  if b
    then Just <$> exportBrightnessControls intelBacklightConfig client
    else return Nothing

callGetBrightnessIB :: IO (Maybe Brightness)
callGetBrightnessIB = callGetBrightness intelBacklightConfig

matchSignalIB :: (Maybe Brightness -> IO ()) -> IO SignalHandler
matchSignalIB = matchSignal intelBacklightConfig
