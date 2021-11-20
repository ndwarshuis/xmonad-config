--------------------------------------------------------------------------------
-- | DBus module for Intel Backlight control

module XMonad.Internal.DBus.Brightness.IntelBacklight
  ( callGetBrightnessIB
  , matchSignalIB
  , exportIntelBacklight
  , curFileDep
  , maxFileDep
  , blPath
  ) where

import           Data.Int                               (Int32)

import           DBus
import           DBus.Client

import           System.FilePath.Posix

import           XMonad.Internal.DBus.Brightness.Common
import           XMonad.Internal.Dependency
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

curFileDep :: Dependency (IO a)
curFileDep = pathRW curFile

maxFileDep :: Dependency (IO a)
maxFileDep = pathR maxFile

exportIntelBacklight :: Client -> IO BrightnessControls
exportIntelBacklight =
  exportBrightnessControls [curFileDep, maxFileDep] intelBacklightConfig

callGetBrightnessIB :: IO (Maybe Brightness)
callGetBrightnessIB = callGetBrightness intelBacklightConfig

matchSignalIB :: (Maybe Brightness -> IO ()) -> IO SignalHandler
matchSignalIB = matchSignal intelBacklightConfig
