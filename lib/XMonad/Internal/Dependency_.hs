{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeAction
  , AnyFeature(..)
  , DepChoice(..)
  , MaybeX
  , FullDep(..)
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
  , fullDep
  , exe
  , listToAnds
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Identity

-- import           Data.Aeson
import           Data.Bifunctor
import           Data.Either
import           Data.List               (find)
import           Data.Maybe
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

data Feature a = Feature (Feature_ a) (Feature a)
  | NoFeature
  | ConstFeature a

-- TODO this is silly as is, and could be made more useful by representing
-- loglevels
data Warning = Silent | Default

type FeatureX = Feature (X ())

type FeatureIO = Feature (IO ())

data AnyFeature = FX FeatureX | FIO FeatureIO

feature :: String -> Warning -> DepTree a -> Feature a
feature n w t = Feature f NoFeature
  where
    f = Feature_
      { ftrDepTree = t
      , ftrName = n
      , ftrWarning = w
      }

ioFeature :: MonadIO m => Feature (IO b) -> Feature (m b)
ioFeature (ConstFeature a) = ConstFeature $ liftIO a
ioFeature NoFeature = NoFeature
ioFeature (Feature f r)
  = Feature (f {ftrDepTree = liftIO <$> ftrDepTree f}) $ ioFeature r

featureDefault :: String -> DepChoice (FullDep Dependency) -> a -> Feature a
featureDefault n ds x = feature n Default $ GenTree (Single x) ds

featureExe :: MonadIO m => String -> String -> Feature (m ())
featureExe n cmd = featureExeArgs n cmd []

featureExeArgs :: MonadIO m => String -> String -> [String] -> Feature (m ())
featureExeArgs n cmd args =
  featureDefault n (Only $ FullDep (Right False) $ Executable cmd) $ spawnCmd cmd args

featureEndpoint :: String -> BusName -> ObjectPath -> InterfaceName
  -> MemberName -> Maybe Client -> FeatureIO
featureEndpoint name busname path iface mem client = feature name Default
  $ DBusTree (Single cmd) client deps
  where
    cmd c = void $ callMethod c busname path iface mem
    deps = Only $ FullDep (Right False) $ Endpoint busname path iface $ Method_ mem

--------------------------------------------------------------------------------
-- | Dependency Trees
--
-- Dependency trees have two subtypes: general and DBus. The latter require a
-- DBus client to evaluate (and will automatically fail if this is missing).
-- The former can be evaluated independently.

data DepChoice a = And (DepChoice a) (DepChoice a)
  | Or (DepChoice a) (DepChoice a)
  | Only a

data DepChoice_ a b = And_ (b -> b -> b) (DepChoice_ a b) (DepChoice_ a b)
  | Or_ (DepChoice_ a b) (DepChoice_ a b)
  | Only_ a

listToAnds :: a -> [a] -> DepChoice a
listToAnds i = foldr (And . Only) (Only i)

-- listToAnds' :: [a] -> Maybe (DepChoice a)
-- listToAnds' []     = Nothing
-- listToAnds' (x:xs) = Just $ foldr (And . Only) (Only x) xs

data DepTree a = GenTree (Action a) (DepChoice (FullDep Dependency))
  | DBusTree (Action (Client -> a)) (Maybe Client) (DepChoice (FullDep DBusDep))

data DepTree_ a b = GenTree_ (Action_ a b) (DepChoice_ (FullDep Dependency) b)
  | DBusTree_ (Action_ (Client -> a) b) (Maybe Client) (DepChoice_ (FullDep DBusDep) b)

instance Functor DepTree where
  fmap f (GenTree a ds)    = GenTree (f <$> a) ds
  fmap f (DBusTree a c ds) = DBusTree (fmap (fmap f) a) c ds

--------------------------------------------------------------------------------
-- | Actions
--
-- Actions have two subtypes: single and double. Single actions are just one
-- independent action. Double actions have one dependent pre-step which the
-- main action consumes (and fails if the pre-step fails).

data Action a = Single a | forall b. Double (b -> a) (IO (Either [String] b))

data Action_ a b = Standalone a | Consumer (b -> a)

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
evalFeature NoFeature = return Nothing
evalFeature (Feature f alt) = do
  procName <- getProgName
  res <- evalTree =<< evalTree' a
  case res of
    Right r -> return $ Just r
    Left l  -> do
      printWarnings procName l
      case alt of
        NoFeature -> return Nothing
        -- TODO make this message better
        next      -> putStrLn "trying alternative" >> evalFeature next

  where
    Feature_ {ftrDepTree = a, ftrName = n, ftrWarning = w} = f
    printWarnings procName es = do
      case w of
        Silent  -> return ()
        Default -> let prefix = n ++ " disabled; "
                       es' = fmap (fmtMsg procName . (prefix ++)) es in
          mapM_ putStrLn es'
    fmtMsg procName msg = unwords [bracket procName, bracket "WARNING", msg]
    bracket s = concat ["[", s, "]"]

mapMDepChoice :: Monad m => (a -> m a) -> (a -> Bool) -> DepChoice a -> m (DepChoice a)
mapMDepChoice f pass = fmap snd . go
  where
    go d@(And a b) = do
      (ra, a') <- go a
      if not ra then return (False, d) else do
        (rb, b') <- go b
        return $ if rb then (True, And a' b') else (False, d)
    go d@(Or a b) = do
      (ra, a') <- go a
      if ra then return (True, Or a' b) else do
        (rb, b') <- go b
        return $ if rb then (True, Or a' b') else (False, d)
    go d@(Only a)  = do
      a' <- f a
      return $ if pass a' then (True, Only a') else (False, d)

-- foldDepChoice :: (a -> Bool) -> DepChoice a -> Bool
-- foldDepChoice get dc = case dc of
--   And a b -> go a && go b
--   Or a b  -> go a || go b
--   Only a  -> get a
--   where
--     go = foldDepChoice get

foldDepChoice' :: Bool -> (a -> Maybe b) -> DepChoice a -> [b]
foldDepChoice' justSucceed get = fromMaybe [] . go []
  where
    go acc (And a b) = Just $ andFun acc a b
    go acc (Or a b)  = Just $ orFun acc a b
    go acc (Only a)  = (:acc) <$> get a
    (andFun, orFun) = if justSucceed then (and', or') else (or', and')
    and' acc a b = case (go acc a, go acc b) of
      (Just a', Just b') -> a' ++ b' ++ acc
      (Just a', Nothing) -> a' ++ acc
      (Nothing, _)       -> acc
    or' acc a b = fromMaybe [] (go acc a) ++ fromMaybe [] (go acc b) ++ acc

foldDepChoiceX :: (a -> Either b c) -> DepChoice_ a c -> Either [b] c
foldDepChoiceX get = go (Left [])
  where
    go acc (And_ f a b) = case (go acc a, go acc b) of
      (Right a', Right b') -> Right $ f a' b'
      (Left a', Right _)   -> toLeft acc a'
      (Right _, Left b')   -> toLeft acc b'
      (Left a', Left b')   -> toLeft acc $ a' ++ b'
    go acc (Or_ a b)    = case (go acc a, go acc b) of
      (Right a', _)      -> Right a'
      (Left _, Right b') -> Right b'
      (Left a', Left b') -> toLeft acc $ a' ++ b'
    go acc (Only_ a)    = first (:fromLeft [] acc) $ get a
    toLeft acc xs = Left $ xs ++ fromLeft [] acc

-- foldDepChoice :: DepChoice a -> (a -> Maybe b) -> [b]
-- foldDepChoice dc f = go [] dc
--   where
--     go acc d = case d of
--       And a b -> do
--         acc'@(a':_) <- go acc a
--         if pass a' then go acc' b else return acc
--       Or a b  -> do
--         acc'@(a':_) <- go acc a
--         if pass a' then return [a'] else go acc' b
--       Only a  -> maybe acc $ f a

-- TODO wet code
evalTree :: DepTree a -> IO (Either [String] a)
evalTree dt = case dt of
  (GenTree a ds) ->
    case foldDep ds of
      [] -> evalAction a
      es -> return $ Left es
  (DBusTree a (Just client) ds) ->
    case foldDep ds of
      [] -> fmap (\f -> f client) <$> evalAction a
      es -> return $ Left es
  (DBusTree _ Nothing _) -> return $ Left ["client not available"]
  where
    foldDep = foldDepChoice' False fullDepMsg
    fullDepMsg (FullDep e _) = either Just (const Nothing) e

evalTreeX :: DepTree_ a b -> IO (Either [String] a)
evalTreeX dt = case dt of
  (GenTree_ a ds) ->
    case foldDep ds of
      [] -> evalAction a
      es -> return $ Left es
  (DBusTree_ a (Just client) ds) ->
    case foldDep ds of
      [] -> fmap (\f -> f client) <$> evalAction a
      es -> return $ Left es
  (DBusTree_ _ Nothing _) -> return $ Left ["client not available"]
  where
    foldDep = foldDepChoiceX fullDepMsg
    fullDepMsg (FullDep e _) = either Just (const Nothing) e

depResult :: FullDep Dependency -> Either String (Maybe b)
depResult (FullDep l@(Left e) d) = Left e
depResult (FullDep _ d)          = Right $ extractDependency d

evalTree' :: DepTree a -> IO (DepTree a)

evalTree' (GenTree a ds) = GenTree a <$> mapMDepChoice eval pass ds
  where
    eval (FullDep _ d) = do
      r <- evalDependency d
      return $ FullDep (maybe (Right True) Left r) d
    pass (FullDep (Right True) _) = True
    pass  _                       = True

evalTree' d@(DBusTree _ Nothing _) = return d
evalTree' (DBusTree a (Just client) ds) = DBusTree a (Just client) <$> mapMDepChoice eval pass ds
  where
    eval (FullDep _ d) = do
      r <- eval' d
      return $ FullDep (maybe (Right True) Left r) d
    eval' (DBusGenDep d) = evalDependency d
    eval' x              = dbusDepSatisfied client x
    pass (FullDep (Right True) _) = True
    pass  _                       = True


evalAction :: Action a -> IO (Either [String] a)
evalAction (Single a)   = return $ Right a
evalAction (Double a b) = fmap a <$> b

-- evalActionX :: Action_ a b -> IO (Either [String] a)
-- evalActionX (Standalone a) = return $ Right a
-- evalActionX (Consumer f)   = fmap a <$> b

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

data FullDep a = FullDep (Either String Bool) a deriving (Functor)

fullDep :: a -> FullDep a
fullDep = FullDep (Right True)

data Dependency = Executable String
  | AccessiblePath FilePath Bool Bool
  | IOTest String (IO (Maybe String))
  | Systemd UnitType String
  | DepFeature AnyFeature

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

exe :: String -> FullDep Dependency
exe = fullDep . Executable

pathR :: String -> FullDep Dependency
pathR n = fullDep $ AccessiblePath n True False

pathW :: String -> FullDep Dependency
pathW n = fullDep $ AccessiblePath n False True

pathRW :: String -> FullDep Dependency
pathRW n = fullDep $ AccessiblePath n True True

systemUnit :: String -> FullDep Dependency
systemUnit = fullDep . Systemd SystemUnit

userUnit :: String -> FullDep Dependency
userUnit = fullDep . Systemd UserUnit

--------------------------------------------------------------------------------
-- | Dependencies (DBus)

data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

data DBusDep =
  Bus BusName
  | Endpoint BusName ObjectPath InterfaceName DBusMember
  | DBusGenDep Dependency

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
evalDependency (DepFeature _)         = undefined
-- TODO add something here to eval a nested feature's dependencies while
-- bypassing the feature itself

extractDependency :: Dependency -> Maybe a
extractDependency (Executable n)         = Nothing
extractDependency (IOTest _ t)           = Nothing
extractDependency (Systemd t n)          = Nothing
extractDependency (AccessiblePath p r w) = Nothing
extractDependency (DepFeature _)         = Nothing

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

dbusDepSatisfied _ (DBusGenDep d) = evalDependency d

--------------------------------------------------------------------------------
-- | Printing dependencies

-- instance ToJSON (Feature_ a) where
--   toJSON Feature_ {}   = undefined

-- instance ToJSON (Feature_ a) where
--   toJSON Feature_ {}   = undefined

-- instance ToJSON (DepTree a) where
--   toJSON (GenTree _ _) = undefined
--   toJSON (DBusTree {}) = undefined

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
--   toJSON (DepFeature _) = undefined

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
depName (DepFeature _)         = "feature: blablabla"

