--------------------------------------------------------------------------------
-- | DBus module for DBus brightness controls

module XMonad.Internal.DBus.Brightness.Common
  ( BrightnessConfig(..)
  , BrightnessControls(..)
  , exportBrightnessControls
  , callGetBrightness
  , matchSignal
  ) where

import           Control.Monad               (void, when)

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

exportBrightnessControls :: RealFrac b => [Dependency] -> BrightnessConfig a b
  -> Client -> IO BrightnessControls
exportBrightnessControls deps bc client = do
  (req, opt) <- checkInstalled deps
  let callBacklight' = createInstalled req opt . callBacklight bc
  when (null req) $
    exportBrightnessControls' bc client
  return $ BrightnessControls
    { bctlMax = callBacklight' memMax
    , bctlMin = callBacklight' memMin
    , bctlInc = callBacklight' memInc
    , bctlDec = callBacklight' memDec
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

callBacklight :: BrightnessConfig a b -> MemberName -> IO ()
callBacklight BrightnessConfig { bcPath = p, bcInterface = i } mem =
  void $ callMethod $ methodCall p i mem

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
