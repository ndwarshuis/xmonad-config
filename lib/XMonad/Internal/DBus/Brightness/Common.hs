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

-- import           Control.Monad               (void)

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
  { bcMin       :: (a, a) -> IO b
  , bcMax       :: (a, a) -> IO b
  , bcDec       :: (a, a) -> IO b
  , bcInc       :: (a, a) -> IO b
  , bcGet       :: (a, a) -> IO b
  , bcMinRaw    :: a
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

brightnessControls :: BrightnessConfig a b -> Maybe Client -> BrightnessControls
brightnessControls bc client =
  BrightnessControls
  { bctlMax = cb "max brightness" memMax
  , bctlMin = cb "min brightness" memMin
  , bctlInc = cb "increase brightness" memInc
  , bctlDec = cb "decrease brightness" memDec
  }
  where
    cb = callBacklight client bc

-- TODO not dry
callGetBrightness :: Num c => BrightnessConfig a b -> Client -> IO (Maybe c)
callGetBrightness BrightnessConfig { bcPath = p, bcInterface = i } client = do
  reply <- callMethod client xmonadBusName p i memGet
  return $ either (const Nothing) bodyGetBrightness reply

-- signalDep :: BrightnessConfig a b -> Dependency
signalDep :: BrightnessConfig a b -> Endpoint
signalDep BrightnessConfig { bcPath = p, bcInterface = i } =
  Endpoint p i $ Signal_ memCur

matchSignal :: Num c => BrightnessConfig a b -> (Maybe c -> IO ()) -> IO SignalHandler
matchSignal BrightnessConfig { bcPath = p, bcInterface = i } cb = do
  client <- connectSession
  -- this connections must remain active
  -- TODO does this need to be cleaned up during shutdown??
  addMatch client brMatcher $ cb . bodyGetBrightness . signalBody
  where
    brMatcher = matchAny
      { matchPath = Just p
      , matchInterface = Just i
      , matchMember = Just memCur
      }

--------------------------------------------------------------------------------
-- | Internal DBus Crap

brightnessExporter :: RealFrac b => [Dependency] -> BrightnessConfig a b
  -> Maybe Client -> FeatureIO
brightnessExporter deps bc@BrightnessConfig { bcName = n } client = Feature
  { ftrMaybeAction = DBusBus_ (exportBrightnessControls' bc) xmonadBusName client deps
  , ftrName = n ++ " exporter"
  , ftrWarning = Default
  }

exportBrightnessControls' :: RealFrac b => BrightnessConfig a b -> Client -> IO ()
exportBrightnessControls' bc client = do
  maxval <- bcGetMax bc -- assume the max value will never change
  let bounds = (bcMinRaw bc, maxval)
  let autoMethod' m f = autoMethod m $ emitBrightness bc client =<< f bc bounds
  let funget = bcGet bc
  export client (bcPath bc) defaultInterface
    { interfaceName = bcInterface bc
    , interfaceMethods =
      [ autoMethod' memMax bcMax
      , autoMethod' memMin bcMin
      , autoMethod' memInc bcInc
      , autoMethod' memDec bcDec
      , autoMethod memGet (round <$> funget bounds :: IO Int32)
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

callBacklight :: Maybe Client -> BrightnessConfig a b -> String -> MemberName -> FeatureIO
callBacklight client BrightnessConfig { bcPath = p, bcInterface = i, bcName = n } controlName m =
  (featureEndpoint xmonadBusName p i m client)
  { ftrName = unwords [n, controlName] }
  -- Feature
  -- { ftrMaybeAction = void $ callMethod client xmonadBusName p i m
  -- , ftrName = unwords [n, controlName]
  -- , ftrWarning = Default
  -- , ftrChildren = [xDbusDep p i $ Method_ m]
  -- }

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
