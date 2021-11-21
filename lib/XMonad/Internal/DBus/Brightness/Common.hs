--------------------------------------------------------------------------------
-- | DBus module for DBus brightness controls

module XMonad.Internal.DBus.Brightness.Common
  ( BrightnessConfig(..)
  , BrightnessControls(..)
  , brightnessControls
  , brightnessExporter
  , callGetBrightness
  , matchSignal
  , signalDep
  ) where

import           Control.Monad               (void)

import           Data.Int                    (Int32)

import           DBus
import           DBus.Client
import qualified DBus.Introspection          as I

import           XMonad.Internal.DBus.Common
import           XMonad.Internal.Dependency

--------------------------------------------------------------------------------
-- | External API
--
-- Define four methods to increase, decrease, maximize, or minimize the
-- brightness. These methods will all return the current brightness as a 32-bit
-- integer and emit a signal with the same brightness value. Additionally, there
-- is one method to get the current brightness.

data BrightnessConfig a b = BrightnessConfig
  { bcMin       :: a -> IO b
  , bcMax       :: a -> IO b
  , bcDec       :: a -> IO b
  , bcInc       :: a -> IO b
  , bcGet       :: a -> IO b
  , bcGetMax    :: IO a
  , bcPath      :: ObjectPath
  , bcInterface :: InterfaceName
  , bcName      :: String
  }

data BrightnessControls = BrightnessControls
  { bctlMax :: FeatureIO
  , bctlMin :: FeatureIO
  , bctlInc :: FeatureIO
  , bctlDec :: FeatureIO
  }

brightnessControls :: BrightnessConfig a b -> BrightnessControls
brightnessControls bc =
  BrightnessControls
  { bctlMax = cb "max brightness" memMax
  , bctlMin = cb "min brightness" memMin
  , bctlInc = cb "increase brightness" memInc
  , bctlDec = cb "decrease brightness" memDec
  }
  where
    cb = callBacklight bc

callGetBrightness :: Num c => BrightnessConfig a b -> IO (Maybe c)
callGetBrightness BrightnessConfig { bcPath = p, bcInterface = i } = do
  reply <- callMethod $ methodCall p i memGet
  return $ reply >>= bodyGetBrightness

signalDep :: BrightnessConfig a b -> Dependency
signalDep BrightnessConfig { bcPath = p, bcInterface = i } =
  DBusEndpoint xmonadBus $ Endpoint p i $ Signal_ memCur

matchSignal :: Num c => BrightnessConfig a b -> (Maybe c -> IO ()) -> IO SignalHandler
matchSignal BrightnessConfig { bcPath = p, bcInterface = i } cb = do
  client <- connectSession
  addMatch client brMatcher $ cb . bodyGetBrightness . signalBody
  -- TODO disconnect here?
  where
    brMatcher = matchAny
      { matchPath = Just p
      , matchInterface = Just i
      , matchMember = Just memCur
      }

--------------------------------------------------------------------------------
-- | Internal DBus Crap

brightnessExporter :: RealFrac b => [Dependency] -> BrightnessConfig a b
  -> Client -> FeatureIO
brightnessExporter deps bc@BrightnessConfig { bcName = n } client = Feature
  { ftrAction = exportBrightnessControls' bc client
  , ftrName = n ++ " exporter"
  , ftrWarning = Default
  , ftrChildren = DBusBus xmonadBus:deps
  }

exportBrightnessControls' :: RealFrac b => BrightnessConfig a b -> Client -> IO ()
exportBrightnessControls' bc client = do
  maxval <- bcGetMax bc -- assume the max value will never change
  let autoMethod' m f = autoMethod m $ emitBrightness bc client =<< f bc maxval
  let funget = bcGet bc
  export client (bcPath bc) defaultInterface
    { interfaceName = bcInterface bc
    , interfaceMethods =
      [ autoMethod' memMax bcMax
      , autoMethod' memMin bcMin
      , autoMethod' memInc bcInc
      , autoMethod' memDec bcDec
      , autoMethod memGet (round <$> funget maxval :: IO Int32)
      ]
    , interfaceSignals = [sig]
    }
  where
    sig = I.Signal
      { I.signalName = memCur
      , I.signalArgs =
        [
          I.SignalArg
          { I.signalArgName = "brightness"
          , I.signalArgType = TypeInt32
          }
        ]
      }

emitBrightness :: RealFrac b => BrightnessConfig a b -> Client -> b -> IO ()
emitBrightness BrightnessConfig{ bcPath = p, bcInterface = i } client cur =
  emit client $ sig { signalBody = [toVariant (round cur :: Int32)] }
  where
    sig = signal p i memCur

callBacklight :: BrightnessConfig a b -> String -> MemberName -> FeatureIO
callBacklight BrightnessConfig { bcPath = p, bcInterface = i, bcName = n } controlName m =
  Feature
  { ftrAction = void $ callMethod $ methodCall p i m
  , ftrName = unwords [n, controlName]
  , ftrWarning = Default
  , ftrChildren = [xDbusDep p i $ Method_ m]
  }

bodyGetBrightness :: Num a => [Variant] -> Maybe a
bodyGetBrightness [b] = fromIntegral <$> (fromVariant b :: Maybe Int32)
bodyGetBrightness _   = Nothing

--------------------------------------------------------------------------------
-- | DBus Members

memCur :: MemberName
memCur = memberName_ "CurrentBrightness"

memGet :: MemberName
memGet = memberName_ "GetBrightness"

memMax :: MemberName
memMax = memberName_ "MaxBrightness"

memMin :: MemberName
memMin = memberName_ "MinBrightness"

memInc :: MemberName
memInc = memberName_ "IncBrightness"

memDec :: MemberName
memDec = memberName_ "DecBrightness"
