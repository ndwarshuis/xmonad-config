{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeAction
  , MaybeX
  , DepTree(..)
  , Action(..)
  , DBusDep(..)
  , FeatureX
  , FeatureIO
  , Feature(..)
  , Feature_(..)
  , Warning(..)
  , Dependency(..)
  , UnitType(..)
  , DBusMember(..)
  , feature
  , ioFeature
  , evalFeature
  , systemUnit
  , userUnit
  , pathR
  , pathW
  , pathRW
  , featureDefault
  , featureExeArgs
  , featureExe
  , featureEndpoint
  , whenSatisfied
  , ifSatisfied
  , executeFeature
  , executeFeature_
  , executeFeatureWith
  , executeFeatureWith_
  , depName
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Identity

-- import           Data.Aeson
import           Data.List               (find)
import           Data.Maybe              (catMaybes, fromMaybe, listToMaybe)
-- import qualified Data.Text               as T

import           DBus
import           DBus.Client
import           DBus.Internal
import qualified DBus.Introspection      as I

import           System.Directory        (findExecutable, readable, writable)
import           System.Environment
import           System.Exit

import           XMonad.Core             (X, io)
import           XMonad.Internal.IO
import           XMonad.Internal.Process
import           XMonad.Internal.Shell

--------------------------------------------------------------------------------
-- | Features
--
-- A 'feature' is composed of a 'dependency tree' which at the root has an
-- 'action' to be performed with a number of 'dependencies' below it.
--
-- NOTE: there is no way to make a feature depend on another feature. This is
-- very complicated to implement and would only be applicable to a few instances
-- (notably the dbus interfaces). In order to implement a dependency tree, use
-- dependencies that target the output/state of another feature; this is more
-- robust anyways, at the cost of being a bit slower.

-- TODO some things to add to make this more feature-ful (lol)
-- - use AndOr types to encode alternative dependencies into the tree
-- - use an Alt data constructor for Features (which will mean "try A before B"
-- - add an Either String Bool to dependency nodes that encodes testing status
--   (where Right False means untested)
-- - add a lens/functor mapper thingy to walk down the tree and update testing
--   status fields
-- - print to JSON
-- - make sum type to hold all type instances of Feature blabla (eg IO and X)
-- - figure out how to make features a dependency of another feature

data Feature_ a = Feature_
  { ftrDepTree :: DepTree a
  , ftrName    :: String
  , ftrWarning :: Warning
  }

data Feature a = Feature (Feature_ a) | ConstFeature a

-- TODO this is silly as is, and could be made more useful by representing
-- loglevels
data Warning = Silent | Default

type FeatureX = Feature (X ())

type FeatureIO = Feature (IO ())

feature :: String -> Warning -> DepTree a -> Feature a
feature n w t = Feature $ Feature_
  { ftrDepTree = t
  , ftrName = n
  , ftrWarning = w
  }

ioFeature :: MonadIO m => Feature (IO b) -> Feature (m b)
ioFeature (ConstFeature a) = ConstFeature $ liftIO a
ioFeature (Feature f)      = Feature $ f {ftrDepTree = liftIO <$> ftrDepTree f}

featureDefault :: String -> [Dependency] -> a -> Feature a
featureDefault n ds x = feature n Default $ GenTree (Single x) ds

featureExe :: MonadIO m => String -> String -> Feature (m ())
featureExe n cmd = featureExeArgs n cmd []

featureExeArgs :: MonadIO m => String -> String -> [String] -> Feature (m ())
featureExeArgs n cmd args =
  featureDefault n [Executable cmd] $ spawnCmd cmd args

featureEndpoint :: String -> BusName -> ObjectPath -> InterfaceName
  -> MemberName -> Maybe Client -> FeatureIO
featureEndpoint name busname path iface mem client = feature name Default
  $ DBusTree (Single cmd) client deps []
  where
    cmd c = void $ callMethod c busname path iface mem
    deps = [Endpoint busname path iface $ Method_ mem]

--------------------------------------------------------------------------------
-- | Dependency Trees
--
-- Dependency trees have two subtypes: general and DBus. The latter require a
-- DBus client to evaluate (and will automatically fail if this is missing).
-- The former can be evaluated independently.

data DepTree a = GenTree (Action a) [Dependency]
  | DBusTree (Action (Client -> a)) (Maybe Client) [DBusDep] [Dependency]

instance Functor DepTree where
  fmap f (GenTree a ds)       = GenTree (f <$> a) ds
  fmap f (DBusTree a c es ds) = DBusTree (fmap (fmap f) a) c es ds

--------------------------------------------------------------------------------
-- | Actions
--
-- Actions have two subtypes: single and double. Single actions are just one
-- independent action. Double actions have one dependent pre-step which the
-- main action consumes (and fails if the pre-step fails).

data Action a = Single a | forall b. Double (b -> a) (IO (Either [String] b))

instance Functor Action where
  fmap f (Single a)   = Single (f a)
  fmap f (Double a b) = Double (f . a) b

--------------------------------------------------------------------------------
-- | Feature evaluation
--
-- Evaluate a feature by testing if its dependencies are satisfied, and return
-- either the action of the feature or 0 or more error messages that signify
-- what dependencies are missing and why.

type MaybeAction a = Maybe a

type MaybeX = MaybeAction (X ())

evalFeature :: Feature a -> IO (MaybeAction a)
evalFeature (ConstFeature x) = return $ Just x
evalFeature (Feature (Feature_{ftrDepTree = a, ftrName = n, ftrWarning = w})) = do
  procName <- getProgName
  res <- evalTree a
  either (printWarnings procName) (return . Just) res
  where
    printWarnings procName es = do
      case w of
        Silent  -> skip
        Default -> let prefix = n ++ " disabled; "
                       es' = fmap (fmtMsg procName . (prefix ++)) es in
          mapM_ putStrLn es'
      return Nothing
    fmtMsg procName msg = unwords [bracket procName, bracket "WARNING", msg]
    bracket s = "[" ++ s ++ "]"

evalTree :: DepTree a -> IO (Either [String] a)

evalTree (GenTree action ds) = do
  es <- catMaybes <$> mapM evalDependency ds
  case es of
    []  -> do
      action' <- evalAction action
      return $ case action' of
        Right f  -> Right f
        Left es' -> Left es'
    es' -> return $ Left es'

evalTree (DBusTree _ Nothing _ _) = return $ Left ["client not available"]
evalTree (DBusTree action (Just client) es ds) = do
  eperrors <- mapM (dbusDepSatisfied client) es
  dperrors <- mapM evalDependency ds
  case catMaybes (eperrors ++ dperrors) of
    []  -> do
      action' <- evalAction action
      return $ case action' of
        Right f  -> Right $ f client
        Left es' -> Left es'
    es' -> return $ Left es'

evalAction :: Action a -> IO (Either [String] a)
evalAction (Single a)   = return $ Right a
evalAction (Double a b) = fmap a <$> b

executeFeatureWith :: MonadIO m => (m a -> m a) -> a -> Feature (IO a) -> m a
executeFeatureWith iof def ftr = do
  a <- io $ evalFeature ftr
  maybe (return def) (iof . io) a

executeFeatureWith_ :: MonadIO m => (m () -> m ()) -> Feature (IO ()) -> m ()
executeFeatureWith_ iof = executeFeatureWith iof ()

executeFeature :: MonadIO m => a -> Feature (IO a) -> m a
executeFeature = executeFeatureWith id

executeFeature_ :: Feature (IO ()) -> IO ()
executeFeature_ = executeFeature ()

whenSatisfied :: Monad m => MaybeAction (m ()) -> m ()
whenSatisfied = flip ifSatisfied skip

ifSatisfied ::  MaybeAction a -> a -> a
ifSatisfied (Just x) _ = x
ifSatisfied _ alt      = alt

--------------------------------------------------------------------------------
-- | Dependencies (General)

data Dependency = Executable String
  | AccessiblePath FilePath Bool Bool
  | IOTest String (IO (Maybe String))
  | Systemd UnitType String

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

pathR :: String -> Dependency
pathR n = AccessiblePath n True False

pathW :: String -> Dependency
pathW n = AccessiblePath n False True

pathRW :: String -> Dependency
pathRW n = AccessiblePath n True True

systemUnit :: String -> Dependency
systemUnit = Systemd SystemUnit

userUnit :: String -> Dependency
userUnit = Systemd UserUnit

--------------------------------------------------------------------------------
-- | Dependencies (DBus)

data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

data DBusDep =
  Bus BusName
  | Endpoint BusName ObjectPath InterfaceName DBusMember
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Dependency evaluation (General)
--
-- Test the existence of dependencies and return either Nothing (which actually
-- means success) or Just <error message>.

evalDependency :: Dependency -> IO (Maybe String)
evalDependency (Executable n)         = exeSatisfied n
evalDependency (IOTest _ t)           = t
evalDependency (Systemd t n)          = unitSatisfied t n
evalDependency (AccessiblePath p r w) = pathSatisfied p r w

exeSatisfied :: String -> IO (Maybe String)
exeSatisfied x = do
  r <- findExecutable x
  return $ case r of
    (Just _) -> Nothing
    _        -> Just $ "executable '" ++ x ++ "' not found"

unitSatisfied :: UnitType -> String -> IO (Maybe String)
unitSatisfied u x = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> Nothing
    _           -> Just $ "systemd " ++ unitType u ++ " unit '" ++ x ++ "' not found"
  where
    cmd = fmtCmd "systemctl" $ ["--user" | u == UserUnit] ++ ["status", x]
    unitType SystemUnit = "system"
    unitType UserUnit   = "user"

pathSatisfied :: FilePath -> Bool -> Bool -> IO (Maybe String)
pathSatisfied p testread testwrite = do
  res <- getPermissionsSafe p
  let msg = permMsg res
  return msg
  where
    testPerm False _ _ = Nothing
    testPerm True f r  = Just $ f r
    permMsg NotFoundError            = Just "file not found"
    permMsg PermError                = Just "could not get permissions"
    permMsg (PermResult r) =
      case (testPerm testread readable r, testPerm testwrite writable r) of
        (Just False, Just False) -> Just "file not readable or writable"
        (Just False, _)          -> Just "file not readable"
        (_, Just False)          -> Just "file not writable"
        _                        -> Nothing

--------------------------------------------------------------------------------
-- | Dependency evaluation (DBus)

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

dbusDepSatisfied :: Client -> DBusDep -> IO (Maybe String)
dbusDepSatisfied client (Bus bus) = do
  ret <- callMethod client queryBus queryPath queryIface queryMem
  return $ case ret of
        Left e    -> Just e
        Right b -> let ns = bodyGetNames b in
          if bus' `elem` ns then Nothing
          else Just $ unwords ["name", singleQuote bus', "not found on dbus"]
  where
    bus' = formatBusName bus
    queryBus = busName_ "org.freedesktop.DBus"
    queryIface = interfaceName_ "org.freedesktop.DBus"
    queryPath = objectPath_ "/"
    queryMem = memberName_ "ListNames"
    bodyGetNames [v] = fromMaybe [] $ fromVariant v :: [String]
    bodyGetNames _   = []

dbusDepSatisfied client (Endpoint busname objpath iface mem) = do
  ret <- callMethod client busname objpath introspectInterface introspectMethod
  return $ case ret of
        Left e     -> Just e
        Right body -> procBody body
  where
    procBody body = let res = findMem =<< I.parseXML objpath =<< fromVariant
                          =<< listToMaybe body in
      case res of
        Just True -> Nothing
        _         -> Just $ fmtMsg' mem
    findMem = fmap (matchMem mem)
      . find (\i -> I.interfaceName i == iface)
      . I.objectInterfaces
    matchMem (Method_ n)   = elemMember n I.methodName I.interfaceMethods
    matchMem (Signal_ n)   = elemMember n I.signalName I.interfaceSignals
    matchMem (Property_ n) = elemMember n I.propertyName I.interfaceProperties
    elemMember n fname fmember = elem n . fmap fname . fmember
    fmtMem (Method_ n)   = "method " ++ singleQuote (formatMemberName n)
    fmtMem (Signal_ n)   = "signal " ++ singleQuote (formatMemberName n)
    fmtMem (Property_ n) = "property " ++ singleQuote n
    fmtMsg' m = unwords
      [ "could not find"
      , fmtMem m
      , "on interface"
      , singleQuote $ formatInterfaceName iface
      , "on bus"
      , formatBusName busname
      ]

--------------------------------------------------------------------------------
-- | Printing dependencies

-- instance ToJSON (DepTree a) where
--   toJSON (GenTree _) = undefined

-- instance ToJSON Dependency where
--   toJSON (Executable n)         = depValue "executable" Nothing n
--   toJSON (IOTest d _)           = depValue "internal" Nothing d
--   toJSON (Systemd t n)          = depValue "systemd" (Just $ tp t) n
--     where
--       tp SystemUnit = "sys"
--       tp UserUnit   = "user"
--   toJSON (AccessiblePath p r w) = depValue "path" perms p
--     where
--       perms = case (r, w) of
--         (True, True)  -> Just "readwrite"
--         (True, False) -> Just "read"
--         (False, True) -> Just "write"
--         _             -> Nothing

-- depValue :: String -> Maybe String -> String -> Value
-- depValue t s n = object
--   [ "type" .= t
--   , "name" .= n
--   , "subtype" .= maybe Null (String . T.pack) s
--   ]

depName :: Dependency -> String
depName (Executable n)         = "executable: " ++ n
depName (IOTest d _)           = "internal: " ++ d
depName (Systemd t n)          = "systemd (" ++ tp t ++ "): "  ++ n
  where
    tp SystemUnit = "sys"
    tp UserUnit   = "user"
depName (AccessiblePath p _ _) = "path: " ++ p

