--------------------------------------------------------------------------------
-- | Common internal DBus functions

module DBus.Internal
  ( addMatchCallback
  , getDBusClient
  , withDBusClient
  , withDBusClient_
  , matchProperty
  , matchProperty'
  , matchPropertyChanged
  , SignalMatch(..)
  , SignalCallback
  , MethodBody
  , withSignalMatch
  , callPropertyGet
  , callMethod
  , callMethod'
  , callGetManagedObjects
  , ObjectTree
  , getManagedObjects
  , omInterface
  , addInterfaceAddedListener
  , addInterfaceRemovedListener
  ) where

import           Control.Exception
import           Control.Monad

import           Data.Bifunctor
import qualified Data.Map.Strict   as M
import           Data.Maybe

import           DBus
import           DBus.Client

--------------------------------------------------------------------------------
-- | Methods

type MethodBody = Either String [Variant]

callMethod' :: Client -> MethodCall -> IO MethodBody
callMethod' cl = fmap (bimap methodErrorMessage methodReturnBody) . call cl

callMethod :: Client -> BusName -> ObjectPath -> InterfaceName -> MemberName
  -> IO MethodBody
callMethod client bus path iface mem =
  callMethod' client (methodCall path iface mem)
  { methodCallDestination = Just bus }

--------------------------------------------------------------------------------
-- | Signals

type SignalCallback = [Variant] -> IO ()

addMatchCallback :: MatchRule -> SignalCallback -> Client -> IO SignalHandler
addMatchCallback rule cb client = addMatch client rule $ cb . signalBody

--------------------------------------------------------------------------------
-- | Properties

propertyInterface :: InterfaceName
propertyInterface = interfaceName_ "org.freedesktop.DBus.Properties"

propertySignal :: MemberName
propertySignal = memberName_ "PropertiesChanged"

callPropertyGet :: BusName -> ObjectPath -> InterfaceName -> String -> Client
  -> IO [Variant]
callPropertyGet bus path iface property client = either (const []) (:[])
  <$> getProperty client (methodCall path iface $ memberName_ property)
    { methodCallDestination = Just bus }

-- TODO actually get the real busname when using this (will involve IO)
matchProperty' :: Maybe ObjectPath -> MatchRule
matchProperty' p = matchAny
  -- NOTE: the sender for signals is usually the unique name (eg :X.Y) not the
  -- requested name (eg "org.something.understandable"). If sender is included
  -- here, likely nothing will match. Solution is to somehow get the unique
  -- name, which I could do, but probably won't
  { matchPath = p
  , matchInterface = Just propertyInterface
  , matchMember = Just propertySignal
  }

matchProperty :: ObjectPath -> MatchRule
matchProperty = matchProperty' . Just

data SignalMatch a = Match a | NoMatch | Failure deriving (Eq, Show)

withSignalMatch :: (Maybe a -> IO ()) -> SignalMatch a -> IO ()
withSignalMatch f (Match x) = f (Just x)
withSignalMatch f Failure   = f Nothing
withSignalMatch _ NoMatch   = return ()

matchPropertyChanged :: IsVariant a => InterfaceName -> String -> [Variant]
  -> SignalMatch a
matchPropertyChanged iface property [i, body, _] =
  let i' = (fromVariant i :: Maybe String)
      b = toMap body in
    case (i', b) of
      (Just i'', Just b') -> if i'' == formatInterfaceName iface then
        maybe NoMatch Match $ fromVariant =<< M.lookup property b'
        else NoMatch
      _                   -> Failure
  where
    toMap v = fromVariant v :: Maybe (M.Map String Variant)
matchPropertyChanged _ _ _ = Failure

--------------------------------------------------------------------------------
-- | Client requests

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

--------------------------------------------------------------------------------
-- | Object Manager

type ObjectTree = M.Map ObjectPath (M.Map String (M.Map String Variant))

omInterface :: InterfaceName
omInterface = interfaceName_ "org.freedesktop.DBus.ObjectManager"

getManagedObjects :: MemberName
getManagedObjects = memberName_ "GetManagedObjects"

callGetManagedObjects :: Client -> BusName -> ObjectPath -> IO ObjectTree
callGetManagedObjects client bus path =
  either (const M.empty) (fromMaybe M.empty . (fromVariant <=< listToMaybe))
  <$> callMethod client bus path omInterface getManagedObjects

omInterfacesAdded :: MemberName
omInterfacesAdded = memberName_ "InterfacesAdded"

omInterfacesRemoved :: MemberName
omInterfacesRemoved = memberName_ "InterfacesRemoved"

-- TODO add busname back to this (use NameGetOwner on org.freedesktop.DBus)
addInterfaceChangedListener :: MemberName -> ObjectPath -> SignalCallback
  -> Client -> IO ()
addInterfaceChangedListener prop path = fmap void . addMatchCallback rule
  where
    rule = matchAny
      { matchPath = Just path
      , matchInterface = Just omInterface
      , matchMember = Just prop
      }

addInterfaceAddedListener :: ObjectPath -> SignalCallback -> Client -> IO ()
addInterfaceAddedListener = addInterfaceChangedListener omInterfacesAdded

addInterfaceRemovedListener :: ObjectPath -> SignalCallback -> Client -> IO ()
addInterfaceRemovedListener = addInterfaceChangedListener omInterfacesRemoved
