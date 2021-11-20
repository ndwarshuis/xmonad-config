--------------------------------------------------------------------------------
-- | DBus module for DBus brightness controls

module XMonad.Internal.DBus.Brightness.Common
  ( BrightnessConfig(..)
  , BrightnessControls(..)
  , exportBrightnessControls
  , callGetBrightness
  , matchSignal
  ) where

import           Control.Monad               (void)

import           Data.Int                    (Int32)

import           DBus
import           DBus.Client

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
  }

data BrightnessControls = BrightnessControls
  { bctlMax :: MaybeExe (IO ())
  , bctlMin :: MaybeExe (IO ())
  , bctlInc :: MaybeExe (IO ())
  , bctlDec :: MaybeExe (IO ())
  }

exportBrightnessControls :: RealFrac b => [Dependency (IO ())] -> BrightnessConfig a b
  -> Client -> IO BrightnessControls
exportBrightnessControls deps bc client =
  initControls client (brightnessExporter deps bc) controls
  where
    controls exporter = do
      let callBacklight' = evalFeature . callBacklight bc exporter
      mx <- callBacklight' memMax
      mn <- callBacklight' memMin
      ic <- callBacklight' memInc
      dc <- callBacklight' memDec
      return $ BrightnessControls
        { bctlMax = mx
        , bctlMin = mn
        , bctlInc = ic
        , bctlDec = dc
        }

callGetBrightness :: Num c => BrightnessConfig a b -> IO (Maybe c)
callGetBrightness BrightnessConfig { bcPath = p, bcInterface = i } = do
  reply <- callMethod $ methodCall p i memGet
  return $ reply >>= bodyGetBrightness

matchSignal :: Num c => BrightnessConfig a b -> (Maybe c -> IO ()) -> IO SignalHandler
matchSignal BrightnessConfig { bcPath = p, bcInterface = i } cb = do
  client <- connectSession
  addMatch client brMatcher $ cb . bodyGetBrightness . signalBody
  where
    brMatcher = matchAny
      { matchPath = Just p
      , matchInterface = Just i
      , matchMember = Just memCur
      }

--------------------------------------------------------------------------------
-- | Internal DBus Crap

-- exportBrightnessControls' :: RealFrac b => BrightnessConfig a b -> Client -> IO ()
-- exportBrightnessControls' bc client = do
--   maxval <- bcGetMax bc -- assume the max value will never change
--   let autoMethod' m f = autoMethod m $ emitBrightness bc client =<< f bc maxval
--   let funget = bcGet bc
--   export client (bcPath bc) defaultInterface
--     { interfaceName = bcInterface bc
--     , interfaceMethods =
--       [ autoMethod' memMax bcMax
--       , autoMethod' memMin bcMin
--       , autoMethod' memInc bcInc
--       , autoMethod' memDec bcDec
--       , autoMethod memGet (round <$> funget maxval :: IO Int32)
--       ]
--     }

brightnessExporter :: RealFrac b => [Dependency (IO ())]
  -> BrightnessConfig a b -> Client -> Feature (IO ()) (IO ())
brightnessExporter deps bc client = Feature
  { ftrAction = exportBrightnessControls' bc client
  , ftrSilent = False
  , ftrChildren = deps
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
    }

emitBrightness :: RealFrac b => BrightnessConfig a b -> Client -> b -> IO ()
emitBrightness BrightnessConfig{ bcPath = p, bcInterface = i } client cur =
  emit client $ sig { signalBody = [toVariant (round cur :: Int32)] }
  where
    sig = signal p i memCur

-- callBacklight :: BrightnessConfig a b -> MemberName -> IO ()
-- callBacklight BrightnessConfig { bcPath = p, bcInterface = i } mem =
--   void $ callMethod $ methodCall p i mem

callBacklight :: BrightnessConfig a b -> Feature (IO ()) (IO ()) -> MemberName
  -> Feature (IO ()) (IO ())
callBacklight BrightnessConfig { bcPath = p, bcInterface = i } exporter mem =
  Feature
  { ftrAction = void $ callMethod $ methodCall p i mem
  , ftrSilent = False
  , ftrChildren = [SubFeature exporter]
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
