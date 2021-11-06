--------------------------------------------------------------------------------
-- | DBus module for Intel Backlight control

module XMonad.Internal.DBus.IntelBacklight
  ( callGetBrightness
  , exportIntelBacklight
  , matchSignal
  , hasBacklight
  , blPath
  , BacklightControls(..)
  ) where

import           Control.Monad               (void)

import           Data.Either
import           Data.Int                    (Int32)

import           DBus
import           DBus.Client

import           System.FilePath.Posix

import           XMonad.Internal.DBus.Common
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
--
-- Define four methods to increase, decrease, maximize, or minimize the
-- brightness. These methods will all return the current brightness as a 32-bit
-- integer and emit a signal with the same brightness value. Additionally, there
-- is one method to get the current brightness.

blPath :: ObjectPath
blPath = objectPath_ "/intelbacklight"

interface :: InterfaceName
interface = interfaceName_ "org.xmonad.Brightness"

memCurrentBrightness :: MemberName
memCurrentBrightness = memberName_ "CurrentBrightness"

memGetBrightness :: MemberName
memGetBrightness = memberName_ "GetBrightness"

memMaxBrightness :: MemberName
memMaxBrightness = memberName_ "MaxBrightness"

memMinBrightness :: MemberName
memMinBrightness = memberName_ "MinBrightness"

memIncBrightness :: MemberName
memIncBrightness = memberName_ "IncBrightness"

memDecBrightness :: MemberName
memDecBrightness = memberName_ "DecBrightness"

brSignal :: Signal
brSignal = signal blPath interface memCurrentBrightness
  -- { signalDestination = Just "org.xmonad" }

brMatcher :: MatchRule
brMatcher = matchAny
  { matchPath = Just blPath
  , matchInterface = Just interface
  , matchMember = Just memCurrentBrightness
  }

callBacklight :: MemberName -> IO ()
callBacklight method = void $ callMethod $ methodCall blPath interface method

bodyGetBrightness :: [Variant] -> Maybe Brightness
bodyGetBrightness [b] = fromIntegral <$> (fromVariant b :: Maybe Int32)
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
  if b then exportIntelBacklight' client >> return (Just bc) else return Nothing
  where
    bc =  BacklightControls
      { backlightMax = callMaxBrightness
      , backlightMin = callMinBrightness
      , backlightUp = callIncBrightness
      , backlightDown = callDecBrightness
      }

exportIntelBacklight' :: Client -> IO ()
exportIntelBacklight' client = do
  maxval <- getMaxRawBrightness -- assume the max value will never change
  let emit' f = emitBrightness client =<< f maxval
  export client blPath defaultInterface
    { interfaceName = interface
    , interfaceMethods =
      [ autoMethod memMaxBrightness $ emit' maxBrightness
      , autoMethod memMinBrightness $ emit' minBrightness
      , autoMethod memIncBrightness $ emit' incBrightness
      , autoMethod memDecBrightness $ emit' decBrightness
      , autoMethod memGetBrightness (round <$> getBrightness maxval :: IO Int32)
      ]
    }

emitBrightness :: Client -> Brightness -> IO ()
emitBrightness client cur = emit client
  $ brSignal { signalBody = [toVariant (round cur :: Int32)] }

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
  reply <- callMethod $ methodCall blPath interface memGetBrightness
  return $ reply >>= bodyGetBrightness

matchSignal :: (Maybe Brightness -> IO ()) -> IO SignalHandler
matchSignal cb = do
  client <- connectSession
  addMatch client brMatcher $ cb . bodyGetBrightness . signalBody
