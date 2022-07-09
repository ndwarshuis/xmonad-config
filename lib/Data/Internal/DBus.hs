--------------------------------------------------------------------------------
-- | Common internal DBus functions

module Data.Internal.DBus
  ( SafeClient(..)
  , SysClient(..)
  , SesClient(..)
  , addMatchCallback
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
-- | Type-safe client

class SafeClient c where
  toClient :: c -> Client

  getDBusClient :: IO (Maybe c)

  disconnectDBusClient :: c -> IO ()
  disconnectDBusClient = disconnect . toClient

  withDBusClient :: (c -> IO a) -> IO (Maybe a)
  withDBusClient f = do
    client <- getDBusClient
    forM client $ \c -> do
      r <- f c
      disconnect (toClient c)
      return r

  withDBusClient_ :: (c -> IO ()) -> IO ()
  withDBusClient_ = void . withDBusClient

  fromDBusClient :: (c -> a) -> IO (Maybe a)
  fromDBusClient f = withDBusClient (return . f)

newtype SysClient = SysClient Client

instance SafeClient SysClient where
  toClient (SysClient cl) = cl

  getDBusClient = fmap SysClient <$> getDBusClient' True

newtype SesClient = SesClient Client

instance SafeClient SesClient where
  toClient (SesClient cl) = cl

  getDBusClient = fmap SesClient <$> getDBusClient' False

getDBusClient' :: Bool -> IO (Maybe Client)
getDBusClient' sys = do
  res <- try $ if sys then connectSystem else connectSession
  case res of
    Left e  -> putStrLn (clientErrorMessage e) >> return Nothing
    Right c -> return $ Just c

--------------------------------------------------------------------------------
-- | Methods

type MethodBody = Either String [Variant]

callMethod' :: SafeClient c => c -> MethodCall -> IO MethodBody
callMethod' cl = fmap (bimap methodErrorMessage methodReturnBody)
  . call (toClient cl)

callMethod :: SafeClient c => c -> BusName -> ObjectPath -> InterfaceName
  -> MemberName -> IO MethodBody
callMethod client bus path iface = callMethod' client . methodCallBus bus path iface

methodCallBus :: BusName -> ObjectPath -> InterfaceName -> MemberName -> MethodCall
methodCallBus b p i m = (methodCall p i m)
    { methodCallDestination = Just b }

--------------------------------------------------------------------------------
-- | Bus names

dbusInterface :: InterfaceName
dbusInterface = interfaceName_ "org.freedesktop.DBus"

callGetNameOwner :: SafeClient c => c -> BusName -> IO (Maybe BusName)
callGetNameOwner cl name = bodyToMaybe <$> callMethod' cl mc
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

addMatchCallback :: SafeClient c => MatchRule -> SignalCallback -> c
  -> IO SignalHandler
addMatchCallback rule cb cl = addMatch (toClient cl) rule $ cb . signalBody

matchSignal :: Maybe BusName -> Maybe ObjectPath -> Maybe InterfaceName
  -> Maybe MemberName -> MatchRule
matchSignal b p i m = matchAny
  { matchPath = p
  , matchSender = b
  , matchInterface = i
  , matchMember = m
  }

matchSignalFull :: SafeClient c => c -> BusName -> Maybe ObjectPath
  -> Maybe InterfaceName -> Maybe MemberName -> IO (Maybe MatchRule)
matchSignalFull client b p i m =
  fmap (\o -> matchSignal (Just o) p i m) <$> callGetNameOwner client b

--------------------------------------------------------------------------------
-- | Properties

propertyInterface :: InterfaceName
propertyInterface = interfaceName_ "org.freedesktop.DBus.Properties"

propertySignal :: MemberName
propertySignal = memberName_ "PropertiesChanged"

callPropertyGet :: SafeClient c => BusName -> ObjectPath -> InterfaceName
  -> MemberName -> c -> IO [Variant]
callPropertyGet bus path iface property cl = fmap (either (const []) (:[]))
  $ getProperty (toClient cl) $ methodCallBus bus path iface property

matchProperty :: Maybe BusName -> Maybe ObjectPath -> MatchRule
matchProperty b p =
  matchSignal b p (Just propertyInterface) (Just propertySignal)

matchPropertyFull :: SafeClient c => c -> BusName -> Maybe ObjectPath
  -> IO (Maybe MatchRule)
matchPropertyFull cl b p =
  matchSignalFull cl b p (Just propertyInterface) (Just propertySignal)

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

callGetManagedObjects :: SafeClient c => c -> BusName -> ObjectPath
  -> IO ObjectTree
callGetManagedObjects cl bus path =
  either (const M.empty) (fromMaybe M.empty . fromSingletonVariant)
  <$> callMethod cl bus path omInterface getManagedObjects

addInterfaceChangedListener :: SafeClient c => BusName -> MemberName
  -> ObjectPath -> SignalCallback -> c -> IO (Maybe SignalHandler)
addInterfaceChangedListener bus prop path sc cl = do
  rule <- matchSignalFull cl bus (Just path) (Just omInterface) (Just prop)
  forM rule $ \r -> addMatchCallback r sc cl

addInterfaceAddedListener :: SafeClient c => BusName -> ObjectPath
  -> SignalCallback -> c -> IO (Maybe SignalHandler)
addInterfaceAddedListener bus =
  addInterfaceChangedListener bus omInterfacesAdded

addInterfaceRemovedListener :: SafeClient c => BusName -> ObjectPath
  -> SignalCallback -> c -> IO (Maybe SignalHandler)
addInterfaceRemovedListener bus =
  addInterfaceChangedListener bus omInterfacesRemoved
