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
import           DBus.Internal
import qualified DBus.Introspection          as I

import           XMonad.Core                 (io)
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
  { bctlMax :: SometimesIO
  , bctlMin :: SometimesIO
  , bctlInc :: SometimesIO
  , bctlDec :: SometimesIO
  }

brightnessControls :: XPQuery -> BrightnessConfig a b -> Maybe Client
  -> BrightnessControls
brightnessControls q bc client =
  BrightnessControls
  { bctlMax = cb "max brightness" memMax
  , bctlMin = cb "min brightness" memMin
  , bctlInc = cb "increase brightness" memInc
  , bctlDec = cb "decrease brightness" memDec
  }
  where
    cb = callBacklight q client bc

callGetBrightness :: Num c => BrightnessConfig a b -> Client -> IO (Maybe c)
callGetBrightness BrightnessConfig { bcPath = p, bcInterface = i } client =
  either (const Nothing) bodyGetBrightness
  <$> callMethod client xmonadBusName p i memGet

signalDep :: BrightnessConfig a b -> DBusDependency_
signalDep BrightnessConfig { bcPath = p, bcInterface = i } =
  Endpoint xmonadBusName p i $ Signal_ memCur

matchSignal :: Num c => BrightnessConfig a b -> (Maybe c -> IO ()) -> Client -> IO ()
matchSignal BrightnessConfig { bcPath = p, bcInterface = i } cb =
  void . addMatchCallback brMatcher (cb . bodyGetBrightness)
  where
    -- TODO add busname to this
    brMatcher = matchAny
      { matchPath = Just p
      , matchInterface = Just i
      , matchMember = Just memCur
      }

--------------------------------------------------------------------------------
-- | Internal DBus Crap

brightnessExporter :: RealFrac b => XPQuery -> [IODependency_]
  -> BrightnessConfig a b -> Maybe Client -> SometimesIO
brightnessExporter q deps bc@BrightnessConfig { bcName = n } cl =
  Sometimes (n ++ " DBus Interface") q [Subfeature root "exporter"]
  where
    root = DBusRoot_ (exportBrightnessControls' bc) tree cl
    tree = listToAnds (Bus xmonadBusName) $ fmap DBusIO deps

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

callBacklight :: XPQuery -> Maybe Client -> BrightnessConfig a b -> String
  -> MemberName -> SometimesIO
callBacklight q cl BrightnessConfig { bcPath = p
                                    , bcInterface = i
                                    , bcName = n } controlName m =
  Sometimes (unwords [n, controlName]) q [Subfeature root "method call"]
  where
    root = DBusRoot_ cmd (Only_ $ Endpoint xmonadBusName p i $ Method_ m) cl
    cmd c = io $ void $ callMethod c xmonadBusName p i m

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
