--------------------------------------------------------------------------------
-- | DBus module for Intel Backlight control

module XMonad.Internal.DBus.Brightness.IntelBacklight
  ( callGetBrightnessIB
  , matchSignalIB
  , exportIntelBacklight
  , intelBacklightControls
  , intelBacklightSignalDep
  , blPath
  ) where

import           Data.Int                               (Int32)

import           DBus

import           System.FilePath.Posix

import           XMonad.Internal.DBus.Brightness.Common
import           XMonad.Internal.Dependency
import           XMonad.Internal.IO

--------------------------------------------------------------------------------
-- | Low level sysfs functions
--
type Brightness = Float

type RawBrightness = Int32

type RawBounds = (RawBrightness, RawBrightness)

steps :: Int
steps = 16

minRawBrightness :: RawBrightness
minRawBrightness = 1

backlightDir :: FilePath
backlightDir = "/sys/class/backlight/intel_backlight/"

maxFile :: FilePath
maxFile = backlightDir </> "max_brightness"

curFile :: FilePath
curFile = backlightDir </> "brightness"

getMaxRawBrightness :: IO RawBrightness
getMaxRawBrightness = readInt maxFile

getBrightness :: RawBounds -> IO Brightness
getBrightness bounds = readPercent bounds curFile

minBrightness :: RawBounds -> IO Brightness
minBrightness bounds = writePercentMin bounds curFile

maxBrightness :: RawBounds -> IO Brightness
maxBrightness bounds = writePercentMax bounds curFile

incBrightness :: RawBounds -> IO Brightness
incBrightness = incPercent steps curFile

decBrightness :: RawBounds -> IO Brightness
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
  , bcMinRaw = minRawBrightness
  , bcPath = blPath
  , bcInterface = interface
  , bcName = "Intel backlight"
  }

--------------------------------------------------------------------------------
-- | Exported haskell API

curFileDep :: IODependency_
curFileDep = pathRW curFile []

maxFileDep :: IODependency_
maxFileDep = pathR maxFile []

intelBacklightSignalDep :: DBusDependency_ SesClient
intelBacklightSignalDep = signalDep intelBacklightConfig

exportIntelBacklight :: Maybe SesClient -> SometimesIO
exportIntelBacklight = brightnessExporter xpfIntelBacklight []
  [curFileDep, maxFileDep] intelBacklightConfig

intelBacklightControls :: Maybe SesClient -> BrightnessControls
intelBacklightControls = brightnessControls xpfIntelBacklight intelBacklightConfig

callGetBrightnessIB :: SesClient -> IO (Maybe Brightness)
callGetBrightnessIB = callGetBrightness intelBacklightConfig . toClient

matchSignalIB :: (Maybe Brightness -> IO ()) -> SesClient -> IO ()
matchSignalIB cb = matchSignal intelBacklightConfig cb . toClient
