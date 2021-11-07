--------------------------------------------------------------------------------
-- | DBus module for X11 screensave/DPMS control

module XMonad.Internal.DBus.Screensaver
  ( exportScreensaver
  , callToggle
  , callQuery
  , matchSignal
  , ssPath
  , SSControls(..)
  ) where

import           Control.Monad               (void)

import           DBus
import           DBus.Client

import           Graphics.X11.XScreenSaver
import           Graphics.X11.Xlib.Display

import           XMonad.Internal.DBus.Common
import           XMonad.Internal.Dependency
import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Low-level functions

type SSState = Bool -- true is enabled

toggle :: IO SSState
toggle = do
  st <- query
  -- TODO figure out how not to do this with shell commands
  void $ createProcess' $ proc "xset" $ "s" : args st
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

newtype SSControls = SSControls { ssToggle :: IO () }

exportScreensaver :: Client -> IO (MaybeExe SSControls)
exportScreensaver client = do
  d <- depInstalled $ depData dep
  if d then flip Installed [] <$> exportScreensaver' client
    else return $ Missing [depData dep] []
  where
    dep = exe "xset"

exportScreensaver' :: Client -> IO SSControls
exportScreensaver' client = do
  export client ssPath defaultInterface
    { interfaceName = interface
    , interfaceMethods =
      [ autoMethod memToggle $ emitState client =<< toggle
      , autoMethod memQuery query
      ]
    }
  return $ SSControls { ssToggle = callToggle }

callToggle :: IO ()
callToggle = void $ callMethod $ methodCall ssPath interface memToggle

callQuery :: IO (Maybe SSState)
callQuery = do
  reply <- callMethod $ methodCall ssPath interface memQuery
  return $ reply >>= bodyGetCurrentState

matchSignal :: (Maybe SSState -> IO ()) -> IO SignalHandler
matchSignal cb = addMatchCallback ruleCurrentState $ cb . bodyGetCurrentState
