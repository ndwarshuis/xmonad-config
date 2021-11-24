--------------------------------------------------------------------------------
-- | Common internal DBus functions

module XMonad.Internal.DBus.Common
  ( addMatchCallback
  , getDBusClient
  , withDBusClient
  , withDBusClient_
  , withDBusClientConnection_
  , matchProperty
  , xmonadBusName
  , matchPropertyChanged
  , SignalMatch(..)
  , callPropertyGet
  ) where

import           Control.Exception
import           Control.Monad

import qualified Data.Map.Strict   as M

import           DBus
import           DBus.Client

xmonadBusName :: BusName
xmonadBusName = busName_ "org.xmonad"

-- | Bind a callback to a signal match rule
addMatchCallback :: MatchRule -> ([Variant] -> IO ()) -> Client -> IO ()
addMatchCallback rule cb client = void $ addMatch client rule $ cb . signalBody

getDBusClient :: Bool -> IO (Maybe Client)
getDBusClient sys = do
  res <- try $ if sys then connectSystem else connectSession
  case res of
    Left e  -> putStrLn (clientErrorMessage e) >> return Nothing
    Right c -> return $ Just c

withDBusClient :: Bool -> (Client -> a) -> IO (Maybe a)
withDBusClient sys f = do
  client <- getDBusClient sys
  let r = f <$> client
  mapM_ disconnect client
  return r

withDBusClient_ :: Bool -> (Client -> IO ()) -> IO ()
withDBusClient_ sys f = do
  client <- getDBusClient sys
  mapM_ f client
  mapM_ disconnect client

withDBusClientConnection_ :: Bool -> (Client -> IO ()) -> IO ()
withDBusClientConnection_ sys f = do
  client <- getDBusClient sys
  mapM_ f client

propertyInterface :: InterfaceName
propertyInterface = interfaceName_ "org.freedesktop.DBus.Properties"

propertySignal :: MemberName
propertySignal = memberName_ "PropertiesChanged"

matchProperty :: ObjectPath -> MatchRule
matchProperty p = matchAny
  -- NOTE: the sender for signals is usually the unique name (eg :X.Y) not the
  -- requested name (eg "org.something.understandable"). If sender is included
  -- here, likely nothing will match. Solution is to somehow get the unique
  -- name, which I could do, but probably won't
  { matchPath = Just p
  , matchInterface = Just propertyInterface
  , matchMember = Just propertySignal
  }

data SignalMatch a = Match a | NoMatch | Failure deriving (Eq, Show)

matchPropertyChanged :: InterfaceName -> String -> (Variant -> Maybe a)
  -> [Variant] -> SignalMatch a
matchPropertyChanged iface property f [i, body, _] =
  let i' = (fromVariant i :: Maybe String)
      b = toMap body in
    case (i', b) of
      (Just i'', Just b') -> if i'' == formatInterfaceName iface then
        maybe NoMatch Match $ f =<< M.lookup property b'
        else NoMatch
      _                   -> Failure
  where
    toMap v = fromVariant v :: Maybe (M.Map String Variant)
matchPropertyChanged _ _ _ _ = Failure

callPropertyGet :: BusName -> ObjectPath -> InterfaceName -> String -> Client
  -> IO [Variant]
callPropertyGet bus path iface property client = either (const []) (:[])
  <$> getProperty client (methodCall path iface $ memberName_ property)
    { methodCallDestination = Just bus }
