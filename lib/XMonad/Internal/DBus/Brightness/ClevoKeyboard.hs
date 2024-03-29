--------------------------------------------------------------------------------
-- | DBus module for Clevo Keyboard control

module XMonad.Internal.DBus.Brightness.ClevoKeyboard
  ( callGetBrightnessCK
  , matchSignalCK
  , exportClevoKeyboard
  , clevoKeyboardControls
  , clevoKeyboardSignalDep
  , blPath
  ) where

import           Control.Monad                          (when)

import           Data.Int                               (Int32)
import           Data.Internal.DBus
import           Data.Internal.Dependency

import           DBus

import           System.FilePath.Posix

import           XMonad.Internal.DBus.Brightness.Common
import           XMonad.Internal.IO

--------------------------------------------------------------------------------
-- | Low level sysfs functions
--
type Brightness = Float

type RawBrightness = Int32

type RawBounds = (RawBrightness, RawBrightness)

steps :: Int
steps = 16

-- assume this is hardcoded into the driver and will never change
maxRawBrightness :: RawBrightness
maxRawBrightness = 255

minRawBrightness :: RawBrightness
minRawBrightness = 0

backlightDir :: FilePath
backlightDir = "/sys/devices/platform/tuxedo_keyboard"

stateFile :: FilePath
stateFile = backlightDir </> "state"

stateChange :: Bool -> IO ()
stateChange = writeBool stateFile

stateOn :: IO ()
stateOn = stateChange True

stateOff :: IO ()
stateOff = stateChange False

brightnessFile :: FilePath
brightnessFile = backlightDir </> "brightness"

getBrightness :: RawBounds -> IO Brightness
getBrightness bounds = readPercent bounds brightnessFile

minBrightness :: RawBounds -> IO Brightness
minBrightness bounds = do
  b <- writePercentMin bounds brightnessFile
  stateOff
  return b

maxBrightness :: RawBounds -> IO Brightness
maxBrightness bounds = stateOn >> writePercentMax bounds brightnessFile

incBrightness :: RawBounds -> IO Brightness
incBrightness bounds = stateOn >> incPercent steps brightnessFile bounds

decBrightness :: RawBounds -> IO Brightness
decBrightness bounds = do
  b <- decPercent steps brightnessFile bounds
  when (b == 0) stateOff
  return b

--------------------------------------------------------------------------------
-- | DBus interface

blPath :: ObjectPath
blPath = objectPath_ "/clevo_keyboard"

interface :: InterfaceName
interface = interfaceName_ "org.xmonad.Brightness"

clevoKeyboardConfig :: BrightnessConfig RawBrightness Brightness
clevoKeyboardConfig = BrightnessConfig
  { bcMin = minBrightness
  , bcMax = maxBrightness
  , bcInc = incBrightness
  , bcDec = decBrightness
  , bcGet = getBrightness
  , bcGetMax = return maxRawBrightness
  , bcMinRaw = minRawBrightness
  , bcPath = blPath
  , bcInterface = interface
  , bcName = "Clevo keyboard"
  }

--------------------------------------------------------------------------------
-- | Exported haskell API

stateFileDep :: IODependency_
stateFileDep = pathRW stateFile [Package AUR "tuxedo-keyboard"]

brightnessFileDep :: IODependency_
brightnessFileDep = pathR brightnessFile [Package AUR "tuxedo-keyboard"]

clevoKeyboardSignalDep :: DBusDependency_ SesClient
clevoKeyboardSignalDep = signalDep clevoKeyboardConfig

exportClevoKeyboard :: Maybe SesClient -> SometimesIO
exportClevoKeyboard = brightnessExporter xpfClevoBacklight []
  [stateFileDep, brightnessFileDep] clevoKeyboardConfig

clevoKeyboardControls :: Maybe SesClient -> BrightnessControls
clevoKeyboardControls = brightnessControls xpfClevoBacklight clevoKeyboardConfig

callGetBrightnessCK :: SesClient -> IO (Maybe Brightness)
callGetBrightnessCK = callGetBrightness clevoKeyboardConfig

matchSignalCK :: (Maybe Brightness -> IO ()) -> SesClient -> IO ()
matchSignalCK = matchSignal clevoKeyboardConfig
