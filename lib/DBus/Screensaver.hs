{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | DBus module for X11 screensave/DPMS control

module DBus.Screensaver where

import DBus
import DBus.Client
import DBus.Internal

import Graphics.X11.Xlib.Display
import Graphics.X11.XScreenSaver

import Shell

import XMonad

--------------------------------------------------------------------------------
-- | Low-level functions

type SSState = Bool -- true is enabled

toggle :: IO SSState
toggle = do
  st <- query
  -- TODO figure out how not to do this with shell commands
  spawn $ fmtCmd "xset" $ "s" : args st
  -- TODO this assumes the command succeeds
  return $ not st
  where
    args s = if s then ["off", "-dpms"] else ["on", "+dpms"]

query :: IO SSState
query = do
  dpy <- openDisplay ""
  xssi <- xScreenSaverQueryInfo dpy
  print xssi
  closeDisplay dpy
  return $ case xssi of
    Just XScreenSaverInfo { xssi_state = ScreenSaverDisabled } -> False
    Just XScreenSaverInfo { xssi_state = _ }                   -> True
    -- TODO handle errors better (at least log them?)
    Nothing                                                    -> False

--------------------------------------------------------------------------------
-- | DBus Interface
--
-- Define two methods to enable/disable the screensaver. These methods will
-- emit signals with the state when called. Define another method to get the
-- current state.

ssPath :: ObjectPath
ssPath = "/screensaver"

ssInterface :: InterfaceName
ssInterface = "org.xmonad.Screensaver"

ssState :: MemberName
ssState = "State"

ssToggle :: MemberName
ssToggle = "Toggle"

ssQuery :: MemberName
ssQuery = "Query"

ssSignal :: Signal
ssSignal = signal ssPath ssInterface ssState

ssMatcher :: MatchRule
ssMatcher = matchAny
  { matchPath = Just ssPath
  , matchInterface = Just ssInterface
  , matchMember = Just ssState
  }

exportScreensaver :: Client -> IO ()
exportScreensaver client =
  export client ssPath defaultInterface
    { interfaceName = ssInterface
    , interfaceMethods =
      [ autoMethod ssToggle toggle
      , autoMethod ssQuery query
      ]
    }

callToggle :: IO (Maybe SSState)
callToggle = callMethodEmit mc bodyState sig
  where
    mc = methodCall ssPath ssInterface ssToggle
    sig b = ssSignal { signalBody = b }

bodyState :: [Variant] -> Maybe SSState
bodyState [b] = fromVariant b :: Maybe SSState
bodyState _   = Nothing

callQuery :: IO (Maybe SSState)
callQuery = callMethod mc bodyState
  where
    mc = methodCall ssPath ssInterface ssQuery

matchSignal :: (Maybe SSState -> IO ()) -> IO SignalHandler
matchSignal cb = addMatchCallback ssMatcher $ cb . bodyState
