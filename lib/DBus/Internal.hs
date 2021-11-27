--------------------------------------------------------------------------------
-- | Common internal DBus functions

module DBus.Internal
  ( addMatchCallback
  , getDBusClient
  , fromDBusClient
  , withDBusClient
  , withDBusClient_
  , matchProperty
  , matchPropertyFull
  , matchPropertyChanged
  , SignalMatch(..)
  , SignalCallback
  , MethodBody
  , withSignalMatch
  , callPropertyGet
  , callMethod
  , callMethod'
  , methodCallBus
  , callGetManagedObjects
  , ObjectTree
  , getManagedObjects
  , omInterface
  , addInterfaceAddedListener
  , addInterfaceRemovedListener
  , fromSingletonVariant
  , bodyToMaybe
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
callMethod client bus path iface = callMethod' client . methodCallBus bus path iface

methodCallBus :: BusName -> ObjectPath -> InterfaceName -> MemberName -> MethodCall
methodCallBus b p i m = (methodCall p i m)
    { methodCallDestination = Just b }

--------------------------------------------------------------------------------
-- | Bus names

dbusInterface :: InterfaceName
dbusInterface = interfaceName_ "org.freedesktop.DBus"

callGetNameOwner :: Client -> BusName -> IO (Maybe BusName)
callGetNameOwner client name = bodyToMaybe <$> callMethod' client mc
  where
    mc = (methodCallBus dbusName dbusPath dbusInterface mem)
      { methodCallBody = [toVariant name] }
    mem = memberName_ "GetNameOwner"

--------------------------------------------------------------------------------
-- | Variant parsing

fromSingletonVariant :: IsVariant a => [Variant] -> Maybe a
fromSingletonVariant = fromVariant <=< listToMaybe

bodyToMaybe :: IsVariant a => MethodBody -> Maybe a
bodyToMaybe = either (const Nothing) fromSingletonVariant

--------------------------------------------------------------------------------
-- | Signals

type SignalCallback = [Variant] -> IO ()

addMatchCallback :: MatchRule -> SignalCallback -> Client -> IO SignalHandler
addMatchCallback rule cb client = addMatch client rule $ cb . signalBody

matchSignal :: Maybe BusName -> Maybe ObjectPath -> Maybe InterfaceName
  -> Maybe MemberName -> MatchRule
matchSignal b p i m = matchAny
  { matchPath = p
  , matchSender = b
  , matchInterface = i
  , matchMember = m
  }

matchSignalFull :: Client -> BusName -> Maybe ObjectPath -> Maybe InterfaceName
  -> Maybe MemberName -> IO (Maybe MatchRule)
matchSignalFull client b p i m =
  fmap (\o -> matchSignal (Just o) p i m) <$> callGetNameOwner client b

--------------------------------------------------------------------------------
-- | Properties

propertyInterface :: InterfaceName
propertyInterface = interfaceName_ "org.freedesktop.DBus.Properties"

propertySignal :: MemberName
propertySignal = memberName_ "PropertiesChanged"

callPropertyGet :: BusName -> ObjectPath -> InterfaceName -> MemberName -> Client
  -> IO [Variant]
callPropertyGet bus path iface property client = fmap (either (const []) (:[]))
  $ getProperty client $ methodCallBus bus path iface property

matchProperty :: Maybe BusName -> Maybe ObjectPath -> MatchRule
matchProperty b p =
  matchSignal b p (Just propertyInterface) (Just propertySignal)

matchPropertyFull :: Client -> BusName -> Maybe ObjectPath -> IO (Maybe MatchRule)
matchPropertyFull client b p =
  matchSignalFull client b p (Just propertyInterface) (Just propertySignal)

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

withDBusClient :: Bool -> (Client -> IO a) -> IO (Maybe a)
withDBusClient sys f = do
  client <- getDBusClient sys
  forM client $ \c -> do
    r <- f c
    disconnect c
    return r

withDBusClient_ :: Bool -> (Client -> IO ()) -> IO ()
withDBusClient_ sys = void . withDBusClient sys

fromDBusClient :: Bool -> (Client -> a) -> IO (Maybe a)
fromDBusClient sys f = withDBusClient sys (return . f)

--------------------------------------------------------------------------------
-- | Object Manager

type ObjectTree = M.Map ObjectPath (M.Map String (M.Map String Variant))

omInterface :: InterfaceName
omInterface = interfaceName_ "org.freedesktop.DBus.ObjectManager"

getManagedObjects :: MemberName
getManagedObjects = memberName_ "GetManagedObjects"

omInterfacesAdded :: MemberName
omInterfacesAdded = memberName_ "InterfacesAdded"

omInterfacesRemoved :: MemberName
omInterfacesRemoved = memberName_ "InterfacesRemoved"

callGetManagedObjects :: Client -> BusName -> ObjectPath -> IO ObjectTree
callGetManagedObjects client bus path =
  either (const M.empty) (fromMaybe M.empty . fromSingletonVariant)
  <$> callMethod client bus path omInterface getManagedObjects

addInterfaceChangedListener :: BusName -> MemberName -> ObjectPath
  -> SignalCallback -> Client -> IO (Maybe SignalHandler)
addInterfaceChangedListener bus prop path sc client = do
  rule <- matchSignalFull client bus (Just path) (Just omInterface) (Just prop)
  forM rule $ \r -> addMatchCallback r sc client

addInterfaceAddedListener :: BusName -> ObjectPath -> SignalCallback -> Client
  -> IO (Maybe SignalHandler)
addInterfaceAddedListener bus =
  addInterfaceChangedListener bus omInterfacesAdded

addInterfaceRemovedListener :: BusName -> ObjectPath -> SignalCallback -> Client
  -> IO (Maybe SignalHandler)
addInterfaceRemovedListener bus =
  addInterfaceChangedListener bus omInterfacesRemoved
