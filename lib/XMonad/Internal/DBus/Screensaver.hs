--------------------------------------------------------------------------------
-- | DBus module for X11 screensave/DPMS control

module XMonad.Internal.DBus.Screensaver
  ( exportScreensaver
  , callToggle
  , callQuery
  , matchSignal
  , ssSignalDep
  ) where

import           Control.Monad               (void)

import           DBus
import           DBus.Client
import qualified DBus.Introspection          as I

import           Graphics.X11.XScreenSaver
import           Graphics.X11.Xlib.Display

import           XMonad.Internal.DBus.Common
import           XMonad.Internal.Dependency
import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Low-level functions

type SSState = Bool -- true is enabled

ssExecutable :: String
ssExecutable = "xset"

toggle :: IO SSState
toggle = do
  st <- query
  -- TODO figure out how not to do this with shell commands
  void $ createProcess' $ proc ssExecutable $ "s" : args st
  -- TODO this assumes the command succeeds
  return $ not st
  where
    args s = if s then ["off", "-dpms"] else ["on", "+dpms"]

query :: IO SSState
query = do
  dpy <- openDisplay ""
  xssi <- xScreenSaverQueryInfo dpy
  closeDisplay dpy
  return $ case xssi of
    Just XScreenSaverInfo { xssi_state = ScreenSaverDisabled } -> False
    Just XScreenSaverInfo { xssi_state = _ }                   -> True
    -- TODO handle errors better (at least log them?)
    Nothing                                                    -> False

--------------------------------------------------------------------------------
-- | DBus Interface
--
-- Define a methods to toggle the screensaver. This methods will emit signal
-- with the new state when called. Define another method to get the current
-- state.

ssPath :: ObjectPath
ssPath = objectPath_ "/screensaver"

interface :: InterfaceName
interface = interfaceName_ "org.xmonad.Screensaver"

memState :: MemberName
memState = memberName_ "State"

memToggle :: MemberName
memToggle = memberName_ "Toggle"

memQuery :: MemberName
memQuery = memberName_ "Query"

sigCurrentState :: Signal
sigCurrentState = signal ssPath interface memState

ruleCurrentState :: MatchRule
ruleCurrentState = matchAny
  { matchPath = Just ssPath
  , matchInterface = Just interface
  , matchMember = Just memState
  }

emitState :: Client -> SSState -> IO ()
emitState client sss = emit client $ sigCurrentState { signalBody = [toVariant sss] }

bodyGetCurrentState :: [Variant] -> Maybe SSState
bodyGetCurrentState [b] = fromVariant b :: Maybe SSState
bodyGetCurrentState _   = Nothing

--------------------------------------------------------------------------------
-- | Exported haskell API

exportScreensaver :: Maybe Client -> FeatureIO
exportScreensaver client = Feature
  { ftrMaybeAction = DBusBus_ cmd xmonadBusName client [Executable ssExecutable]
  , ftrName = "screensaver interface"
  , ftrWarning = Default
  }
  where
    cmd cl = export cl ssPath defaultInterface
      { interfaceName = interface
      , interfaceMethods =
        [ autoMethod memToggle $ emitState cl =<< toggle
        , autoMethod memQuery query
        ]
      , interfaceSignals = [sig]
      }
    sig = I.Signal
      { I.signalName = memState
      , I.signalArgs =
        [
          I.SignalArg
          { I.signalArgName = "enabled"
          , I.signalArgType = TypeBoolean
          }
        ]
      }

callToggle :: Maybe Client -> FeatureIO
callToggle client =
  (featureEndpoint xmonadBusName ssPath interface memToggle client)
  { ftrName = "screensaver toggle" }

callQuery :: Client -> IO (Maybe SSState)
callQuery client = do
  reply <- callMethod client xmonadBusName ssPath interface memQuery
  return $ either (const Nothing) bodyGetCurrentState reply

matchSignal :: (Maybe SSState -> IO ()) -> IO SignalHandler
matchSignal cb = addMatchCallback ruleCurrentState $ cb . bodyGetCurrentState

ssSignalDep :: Endpoint
ssSignalDep = Endpoint xmonadBusName ssPath interface $ Signal_ memState
