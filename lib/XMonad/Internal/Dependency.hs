{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  -- feature types
  ( Feature
  , Always(..)
  , Sometimes
  , AlwaysX
  , AlwaysIO
  , SometimesX
  , SometimesIO
  , PostPass(..)
  , Subfeature(..)
  , SubfeatureRoot
  , LogLevel(..)

  -- dependency tree types
  , Root(..)
  , Tree(..)
  , Tree_(..)
  , IODependency(..)
  , IODependency_(..)
  , SystemDependency(..)
  , DBusDependency_(..)
  , DBusMember(..)
  , UnitType(..)
  , Result

  -- dumping
  , dumpFeature
  , dumpAlways
  , dumpSometimes
  , jsonArray
  , JSONQuotable(..)
  , JSONUnquotable(..)
  , JSONMixed(..)

  -- testing
  , evalFeature
  , executeSometimes
  , executeAlways
  , evalAlways
  , evalSometimes

  -- lifting
  , ioSometimes
  , ioAlways

  -- feature construction
  , always1
  , sometimes1
  , sometimesIO
  , sometimesDBus
  , sometimesExe
  , sometimesExeArgs
  , sometimesEndpoint

  -- dependency construction
  , sysExe
  , localExe
  , sysdSystem
  , sysdUser
  , listToAnds
  , toAnd
  , pathR
  , pathRW
  , pathW
  , sysTest
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Identity

import           Data.Bifunctor
import           Data.List
import           Data.Maybe

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
-- | Feature Evaluation
--
-- Here we attempt to build and return the monadic actions encoded by each
-- feature.

-- | Execute an Always immediately
executeAlways :: MonadIO m => Always (m a) -> m a
executeAlways = join . evalAlways

-- | Execute a Sometimes immediately (or do nothing if failure)
executeSometimes :: MonadIO m => Sometimes (m a) -> m (Maybe a)
executeSometimes a = maybe (return Nothing) (fmap Just) =<< evalSometimes a

-- | Possibly return the action of an Always/Sometimes
evalFeature :: MonadIO m => Feature a -> m (Maybe a)
evalFeature (Right a) = Just <$> evalAlways a
evalFeature (Left s)  = evalSometimes s

-- | Possibly return the action of a Sometimes
evalSometimes :: MonadIO m => Sometimes a -> m (Maybe a)
evalSometimes x = io $ either goFail goPass =<< evalSometimesMsg x
  where
    goPass (PostPass a ws) = putErrors ws >> return (Just a)
    goFail es = putErrors es >> return Nothing
    putErrors = mapM_ putStrLn

-- | Return the action of an Always
evalAlways :: MonadIO m => Always a -> m a
evalAlways a = do
  (PostPass x ws) <- evalAlwaysMsg a
  io $ mapM_ putStrLn ws
  return x

--------------------------------------------------------------------------------
-- | Feature status

-- | Dump the status of a Feature
dumpFeature :: Feature a -> IO JSONUnquotable
dumpFeature = either dumpSometimes dumpAlways

-- | Dump the status of an Always to stdout
dumpAlways :: Always a -> IO JSONUnquotable
dumpAlways = go []
  where
    go failed (Option o os) = do
      (s, r) <- dumpSubfeatureRoot o
      if r
        then return $ jsonAlways (Just s) failed $ untested [] os
        else go (s:failed) os
    go failed (Always _) = return $ jsonAlways (Just (UQ "true")) failed []
    untested acc (Always _)    = acc
    untested acc (Option o os) = untested (dataSubfeatureRoot o:acc) os

-- | Dump the status of a Sometimes to stdout
dumpSometimes :: Sometimes a -> IO JSONUnquotable
dumpSometimes = go []
  where
    go failed [] = return $ jsonSometimes Nothing failed []
    go failed (x:xs) = do
      (s, r) <- dumpSubfeatureRoot x
      if r
        then return $ jsonSometimes (Just s) failed $ fmap dataSubfeatureRoot xs
        else go (s:failed) xs

--------------------------------------------------------------------------------
-- | Wrapper types

type AlwaysX = Always (X ())

type AlwaysIO = Always (IO ())

type SometimesX = Sometimes (X ())

type SometimesIO = Sometimes (IO ())

type Feature a = Either (Sometimes a) (Always a)

--------------------------------------------------------------------------------
-- | Feature declaration

-- | Feature that is guaranteed to work
-- This is composed of sub-features that are tested in order, and if all fail
-- the fallback is a monadic action (eg a plain haskell function)
data Always a = Option (SubfeatureRoot a) (Always a) | Always a

-- | Feature that might not be present
-- This is like an Always except it doesn't fall back on a guaranteed monadic
-- action
type Sometimes a = [SubfeatureRoot a]

-- | Individually tested sub-feature data for Always/sometimes
-- The polymorphism allows representing tested and untested states. Includes
-- the 'action' itself to be tested and any auxilary data for describing the
-- sub-feature.
data Subfeature f = Subfeature
  { sfData  :: f
  , sfName  :: String
  , sfLevel :: LogLevel
  }

type SubfeatureRoot a = Subfeature (Root a)

-- | Loglevel at which feature testing should be reported
-- This is currently not used for anything important
data LogLevel = Silent | Error | Warn | Debug deriving (Eq, Show, Ord)

-- | An action and its dependencies
-- May be a plain old monad or be DBus-dependent, in which case a client is
-- needed
data Root a = forall p. IORoot (p -> a) (Tree IODependency IODependency_ p)
  | IORoot_ a (Tree_ IODependency_)
  | forall p. DBusRoot (p -> Client -> a)
    (Tree IODependency DBusDependency_ p) (Maybe Client)
  | DBusRoot_ (Client -> a) (Tree_ DBusDependency_) (Maybe Client)

-- | The dependency tree with rules to merge results
data Tree d d_ p =
  And12 (p -> p -> p) (Tree d d_ p) (Tree d d_ p)
  | And1 (Tree d d_ p) (Tree_ d_)
  | And2 (Tree_ d_) (Tree d d_ p)
  | Or (Tree d d_ p) (Tree d d_ p)
  | Only (d p)

-- | A dependency tree without functions to merge results
data Tree_ d =
  And_ (Tree_ d) (Tree_ d)
  | Or_ (Tree_ d) (Tree_ d)
  | Only_ d

-- | A dependency that only requires IO to evaluate
data IODependency p = IORead String (IO (Result p))
  | forall a. IOAlways (Always a) (a -> p)
  | forall a. IOSometimes (Sometimes a) (a -> p)

-- | A dependency pertaining to the DBus
data DBusDependency_ = Bus BusName
  | Endpoint BusName ObjectPath InterfaceName DBusMember
  | DBusIO IODependency_

-- | A dependency that only requires IO to evaluate (no payload)
data IODependency_ = IOSystem_ SystemDependency
  | forall a. IOSometimes_ (Sometimes a)

data SystemDependency = Executable Bool FilePath
  | AccessiblePath FilePath Bool Bool
  | IOTest String (IO (Maybe String))
  | Systemd UnitType String

-- | The type of a systemd service
data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

-- | Wrapper type to describe and endpoint
data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Tested dependency tree
--
-- The main reason I need this is so I have a "result" I can convert to JSON
-- and dump on the CLI (unless there is a way to make Aeson work inside an IO)

-- | Tested Always feature
data PostAlways a = Primary (SubfeaturePass a) [SubfeatureFail] (Always a)
  | Fallback a [SubfeatureFail]

-- | Tested Sometimes feature
data PostSometimes a = PostSometimes
  { psSuccess :: Maybe (SubfeaturePass a)
  , psFailed  :: [SubfeatureFail]
  }

-- | Passing subfeature
type SubfeaturePass a = Subfeature (PostPass a)

-- | Failed subfeature
type SubfeatureFail = Subfeature PostFail

-- | An action that passed
data PostPass a = PostPass a [String] deriving (Functor)

addMsgs :: PostPass a -> [String] -> PostPass a
addMsgs (PostPass a ms) ms' = PostPass a $ ms ++ ms'

-- | An action that failed
data PostFail = PostFail [String] | PostMissing String

--------------------------------------------------------------------------------
-- | Testing pipeline

evalSometimesMsg :: MonadIO m => Sometimes a -> m (Result a)
evalSometimesMsg x = io $ do
  PostSometimes { psSuccess = s, psFailed = fs } <- testSometimes x
  case s of
    (Just (Subfeature { sfData = p })) -> Right . addMsgs p <$> failedMsgs False fs
    _                                  -> Left <$> failedMsgs True fs

evalAlwaysMsg :: MonadIO m => Always a -> m (PostPass a)
evalAlwaysMsg x = io $ do
  r <- testAlways x
  case r of
    (Primary (Subfeature { sfData = p }) fs _) -> addMsgs p <$> failedMsgs False fs
    (Fallback act fs) -> PostPass act <$> failedMsgs False fs

testAlways :: Always a -> IO (PostAlways a)
testAlways = go []
  where
    go failed (Option fd next) = do
      r <- testSubfeature fd
      case r of
        (Left l)     -> go (l:failed) next
        (Right pass) -> return $ Primary pass failed next
    go failed (Always a) = return $ Fallback a failed

testSometimes :: Sometimes a -> IO (PostSometimes a)
testSometimes = go (PostSometimes Nothing [])
  where
    go ts [] = return ts
    go ts (x:xs) = do
      sf <- testSubfeature x
      case sf of
        (Left l)     -> go (ts { psFailed = l:psFailed ts }) xs
        (Right pass) -> return $ ts { psSuccess = Just pass }

testSubfeature :: SubfeatureRoot a -> IO (Either SubfeatureFail (SubfeaturePass a))
testSubfeature sf@Subfeature{ sfData = t } = do
  t' <- testRoot t
  -- monomorphism restriction :(
  return $ bimap (\n -> sf { sfData = n }) (\n -> sf { sfData = n }) t'

testRoot :: Root a -> IO (Either PostFail (PostPass a))
testRoot r = do
  case r of
    (IORoot a t)              -> go a testIODependency_ testIODependency t
    (IORoot_ a t)             -> go_ a testIODependency_ t
    (DBusRoot a t (Just cl))  -> go (`a` cl) (testDBusDependency_ cl) testIODependency t
    (DBusRoot_ a t (Just cl)) -> go_ (a cl) (testDBusDependency_ cl) t
    _                         -> return $ Left $ PostMissing "client not available"
  where
    go a f_ f t = bimap PostFail (fmap a) <$> testTree f_ f t
    go_ a f_ t = bimap PostFail (PostPass a) <$> testTree_ f_ t

--------------------------------------------------------------------------------
-- | Payloaded dependency testing

type Result p = Either [String] (PostPass p)

testTree :: (d_ -> IO Result_) -> (d p -> IO (Result p)) -> Tree d d_ p
  -> IO (Either [String] (PostPass p))
testTree test_ test = go
  where
    go (And12 f a b) = do
      ra <- go a
      liftRight (\pa -> (and2nd f pa =<<) <$> go b) ra
    go (And1 a b) = do
      ra <- go a
      liftRight (\p -> fmap (addMsgs p) <$> testTree_ test_ b) ra
    go (And2 a b) = do
      ra <- testTree_ test_ a
      liftRight (\wa -> fmap (`addMsgs` wa) <$> go b) ra
    go (Or a b) = do
      ra <- go a
      either (\ea -> fmap (`addMsgs` ea) <$> go b) (return . Right) ra
    go (Only a)    = test a
    and2nd f (PostPass pa wa) (PostPass pb wb) = Right $ PostPass (f pa pb) $ wa ++ wb
    liftRight = either (return . Left)

testIODependency :: IODependency p -> IO (Result p)
testIODependency (IORead _ t)      = t
-- TODO this is a bit odd because this is a dependency that will always
-- succeed, which kinda makes this pointless. The only reason I would want this
-- is if I want to have a built-in logic to "choose" a payload to use in
-- building a higher-level feature
testIODependency (IOAlways a f)    = Right . fmap f <$> evalAlwaysMsg a
testIODependency (IOSometimes x f) = second (fmap f) <$> evalSometimesMsg x

--------------------------------------------------------------------------------
-- | Standalone dependency testing

type Result_ = Either [String] [String]

testTree_ :: (d -> IO Result_) -> Tree_ d -> IO (Either [String] [String])
testTree_ test = go
  where
    go (And_ a b) = either (return . Left) (`test2nd` b) =<< go a
    go (Or_ a b)  = either (`test2nd` b) (return . Right) =<< go a
    go (Only_ a)  = test a
    test2nd ws = fmap ((Right . (ws ++)) =<<) . go

testIODependency_ :: IODependency_ -> IO Result_
testIODependency_ (IOSystem_ s) = maybe (Right []) (Left . (:[])) <$> testSysDependency s
testIODependency_ (IOSometimes_ x) = second (\(PostPass _ ws) -> ws) <$> evalSometimesMsg x

--------------------------------------------------------------------------------
-- | System Dependency Testing

testSysDependency :: SystemDependency -> IO (Maybe String)
testSysDependency (IOTest _ t) = t
testSysDependency (Executable sys bin) = maybe (Just msg) (const Nothing)
  <$> findExecutable bin
  where
    msg = unwords [e, "executable", singleQuote bin, "not found"]
    e = if sys then "system" else "local"
testSysDependency (Systemd t n) = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> Nothing
    _           -> Just $ "systemd " ++ unitType t ++ " unit '" ++ n ++ "' not found"
  where
    cmd = fmtCmd "systemctl" $ ["--user" | t == UserUnit] ++ ["status", n]
testSysDependency (AccessiblePath p r w) = permMsg <$> getPermissionsSafe p
  where
    testPerm False _ _  = Nothing
    testPerm True f res = Just $ f res
    permMsg NotFoundError            = Just "file not found"
    permMsg PermError                = Just "could not get permissions"
    permMsg (PermResult res) =
      case (testPerm r readable res, testPerm w writable res) of
        (Just False, Just False) -> Just "file not readable or writable"
        (Just False, _)          -> Just "file not readable"
        (_, Just False)          -> Just "file not writable"
        _                        -> Nothing


unitType :: UnitType -> String
unitType SystemUnit = "system"
unitType UserUnit   = "user"

--------------------------------------------------------------------------------
-- | DBus Dependency Testing

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

testDBusDependency_ :: Client -> DBusDependency_ -> IO Result_
testDBusDependency_ client (Bus bus) = do
  ret <- callMethod client queryBus queryPath queryIface queryMem
  return $ case ret of
        Left e    -> Left [e]
        Right b -> let ns = bodyGetNames b in
          if bus' `elem` ns then Right []
          else Left [unwords ["name", singleQuote bus', "not found on dbus"]]
  where
    bus' = formatBusName bus
    queryBus = busName_ "org.freedesktop.DBus"
    queryIface = interfaceName_ "org.freedesktop.DBus"
    queryPath = objectPath_ "/"
    queryMem = memberName_ "ListNames"
    bodyGetNames [v] = fromMaybe [] $ fromVariant v :: [String]
    bodyGetNames _   = []

testDBusDependency_ client (Endpoint busname objpath iface mem) = do
  ret <- callMethod client busname objpath introspectInterface introspectMethod
  return $ case ret of
        Left e     -> Left [e]
        Right body -> procBody body
  where
    procBody body = let res = findMem =<< I.parseXML objpath =<< fromVariant
                          =<< listToMaybe body in
      case res of
        Just True -> Right []
        _         -> Left [fmtMsg' mem]
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

testDBusDependency_ _ (DBusIO i) = testIODependency_ i

--------------------------------------------------------------------------------
-- | IO Lifting functions

ioSometimes :: MonadIO m => Sometimes (IO a) -> Sometimes (m a)
ioSometimes = fmap ioSubfeature

ioAlways :: MonadIO m => Always (IO a) -> Always (m a)
ioAlways (Always x)    = Always $ io x
ioAlways (Option sf a) = Option (ioSubfeature sf) $ ioAlways a

ioSubfeature :: MonadIO m => SubfeatureRoot (IO a) -> SubfeatureRoot (m a)
ioSubfeature sf = sf { sfData = ioRoot $ sfData sf }

ioRoot :: MonadIO m => Root (IO a) -> Root (m a)
ioRoot (IORoot a t)       = IORoot (io . a) t
ioRoot (IORoot_ a t)      = IORoot_ (io a) t
ioRoot (DBusRoot a t cl)  = DBusRoot (\p c -> io $ a p c) t cl
ioRoot (DBusRoot_ a t cl) = DBusRoot_ (io . a) t cl

--------------------------------------------------------------------------------
-- | Feature constructors

sometimes1_ :: LogLevel -> String -> Root a -> Sometimes a
sometimes1_ l n t = [Subfeature{ sfData = t, sfName = n, sfLevel = l }]

always1_ :: LogLevel -> String -> Root a -> a -> Always a
always1_ l n t x =
  Option (Subfeature{ sfData = t, sfName = n, sfLevel = l }) (Always x)

sometimes1 :: String -> Root a -> Sometimes a
sometimes1 = sometimes1_ Error

always1 :: String -> Root a -> a -> Always a
always1 = always1_ Error

sometimesIO :: String -> Tree_ IODependency_ -> a -> Sometimes a
sometimesIO n t x = sometimes1 n $ IORoot_ x t

sometimesExe :: MonadIO m => String -> Bool -> FilePath -> Sometimes (m ())
sometimesExe n sys path = sometimesExeArgs n sys path []

sometimesExeArgs :: MonadIO m => String -> Bool -> FilePath -> [String] -> Sometimes (m ())
sometimesExeArgs n sys path args =
  sometimesIO n (Only_ (IOSystem_ $ Executable sys path)) $ spawnCmd path args

sometimesDBus :: Maybe Client -> String -> Tree_ DBusDependency_
  -> (Client -> a) -> Sometimes a
sometimesDBus c n t x = sometimes1 n $ DBusRoot_ x t c

sometimesEndpoint :: MonadIO m => String -> BusName -> ObjectPath -> InterfaceName
  -> MemberName -> Maybe Client -> Sometimes (m ())
sometimesEndpoint name busname path iface mem client =
  sometimesDBus client name deps cmd
  where
    deps = Only_ $ Endpoint busname path iface $ Method_ mem
    cmd c = io $ void $ callMethod c busname path iface mem

--------------------------------------------------------------------------------
-- | Dependency Tree Constructors

listToAnds :: d -> [d] -> Tree_ d
listToAnds i = foldr (And_ . Only_) (Only_ i)

toAnd :: d -> d -> Tree_ d
toAnd a b = And_ (Only_ a) (Only_ b)

--------------------------------------------------------------------------------
-- | IO Dependency Constructors

exe :: Bool -> String -> IODependency_
exe b = IOSystem_ . Executable b

sysExe :: String -> IODependency_
sysExe = exe True

localExe :: String -> IODependency_
localExe = exe False

pathR :: String -> IODependency_
pathR n = IOSystem_ $ AccessiblePath n True False

pathW :: String -> IODependency_
pathW n = IOSystem_ $ AccessiblePath n False True

pathRW :: String -> IODependency_
pathRW n = IOSystem_ $ AccessiblePath n True True

sysd :: UnitType -> String -> IODependency_
sysd u = IOSystem_ . Systemd u

sysdUser :: String -> IODependency_
sysdUser = sysd UserUnit

sysdSystem :: String -> IODependency_
sysdSystem = sysd SystemUnit

sysTest :: String -> IO (Maybe String) -> IODependency_
sysTest n = IOSystem_ . IOTest n

--------------------------------------------------------------------------------
-- | Printing

dumpSubfeatureRoot :: SubfeatureRoot a -> IO (JSONUnquotable, Bool)
dumpSubfeatureRoot Subfeature { sfData = r, sfName = n } =
  first (jsonSubfeature $ Q n) <$> dumpRoot r

dumpRoot :: Root a -> IO (JSONUnquotable, Bool)
dumpRoot (IORoot _ t) = first jsonIORoot <$>
  dumpTree testIODependency testIODependency_ dataIODependency dataIODependency_ t
dumpRoot (IORoot_ _ t) = first jsonIORoot <$>
  dumpTree_ testIODependency_ dataIODependency_ t
dumpRoot (DBusRoot _ t (Just cl)) = first jsonDBusRoot <$>
  dumpTree testIODependency (testDBusDependency_ cl) dataIODependency dataDBusDependency t
dumpRoot (DBusRoot_ _ t (Just cl)) = first jsonDBusRoot <$>
  dumpTree_ (testDBusDependency_ cl) dataDBusDependency t
-- TODO somehow return a message here that these failed
dumpRoot  (DBusRoot _ t Nothing) =
  return (jsonDBusRoot $ dataTree dataIODependency dataDBusDependency t, False)
dumpRoot  (DBusRoot_ _ t Nothing) =
  return (jsonDBusRoot $ dataTree_ dataDBusDependency t, False)

dumpTree :: (d p -> IO (Result p)) -> (d_ -> IO Result_)
  -> (d p -> DependencyData) -> (d_ -> DependencyData) -> Tree d d_ p
  -> IO (JSONUnquotable, Bool)
dumpTree test test_ dd dd_ = go
  where
    go (And12 _ a b) = doAnd go go data' a b
    go (And1 a b)    = doAnd go dump_' (dataTree_ dd_) a b
    go (And2 a b)    = doAnd dump_' go data' a b
    go (Or a b)      = do
      (sa, ra) <- go a
      let j = jsonOr sa
      if ra then return (j $ data' b, ra) else first j <$> go b
    go (Only d)      = do
      r <- fromResult <$> test d
      let (x, y) = dd d
      return (jsonLeaf (Just r) x y, fst r)
    dump_' = dumpTree_ test_ dd_
    data' = dataTree dd dd_
    doAnd fa fb fb_ a b = do
      (sa, ra) <- fa a
      let j = jsonAnd sa
      if ra then first j <$> fb b else return (j $ fb_ b, ra)

dumpTree_ :: (d_ -> IO Result_) -> (d_ -> DependencyData) -> Tree_ d_
  -> IO (JSONUnquotable, Bool)
dumpTree_ test_ dd_ = go
  where
    go (And_ a b) = do
      (sa, ra) <- go a
      let j = jsonAnd sa
      if ra then first j <$> go b else return (j $ dataTree_ dd_ b, ra)
    go (Or_ a b)  = do
      (sa, ra) <- go a
      let j = jsonAnd sa
      if ra then return (j $ dataTree_ dd_ b, ra) else first j <$> go b
    go (Only_ d)  = do
      r <- fromResult_ <$> test_ d
      let (x, y) = dd_ d
      return (jsonLeaf (Just r) x y, fst r)

--------------------------------------------------------------------------------
-- | Dependency data for JSON

type DependencyData = (JSONQuotable, [(String, JSONMixed)])

dataSubfeatureRoot :: SubfeatureRoot a -> JSONUnquotable
dataSubfeatureRoot Subfeature { sfData = r, sfName = n } =
  jsonSubfeature (Q n) $ dataRoot r

dataRoot :: Root a -> JSONUnquotable
dataRoot (IORoot _ t)      = dataTree  dataIODependency dataIODependency_ t
dataRoot (IORoot_ _ t)     = dataTree_ dataIODependency_ t
dataRoot (DBusRoot _ t _)  = dataTree dataIODependency dataDBusDependency t
dataRoot (DBusRoot_ _ t _) = dataTree_ dataDBusDependency t

dataTree :: (d p -> DependencyData) -> (d_ -> DependencyData) -> Tree d d_ p -> JSONUnquotable
dataTree f f_ = go
  where
    go (And12 _ a b) = jsonAnd (go a) (go b)
    go (And1 a b)    = jsonAnd (go a) (dataTree_ f_ b)
    go (And2 a b)    = jsonAnd (dataTree_ f_ a) (go b)
    go (Or a b)      = jsonOr (go a) (go b)
    go (Only d)      = uncurry jsonLeafUntested $ f d

dataTree_ :: (d_ -> DependencyData) -> Tree_ d_ -> JSONUnquotable
dataTree_ f_ = go
  where
    go (And_ a b) = jsonAnd (go a) (go b)
    go (Or_ a b)  = jsonOr (go a) (go b)
    go (Only_ d)  = uncurry jsonLeafUntested $ f_ d

dataIODependency :: IODependency p -> DependencyData
dataIODependency d = case d of
  (IORead n _)      -> (Q "ioread", [("desc", JSON_Q $ Q n)])
  -- TODO make this actually useful (I actually need to name my features)
  (IOSometimes _ _) -> (Q "sometimes", [])
  (IOAlways _ _)    -> (Q "always", [])

dataIODependency_ :: IODependency_ -> DependencyData
dataIODependency_ d = case d of
  (IOSystem_ s)    -> dataSysDependency s
  (IOSometimes_ _) -> (Q "sometimes", [])

dataSysDependency :: SystemDependency -> DependencyData
dataSysDependency d = do
  case d of
    (Executable sys path) -> (Q "executable", [ ("system", JSON_UQ $ jsonBool sys)
                                            , ("path", JSON_Q $ Q path)
                                            ])
    (IOTest desc _) -> (Q "iotest", [("desc", JSON_Q $ Q desc)])
    (AccessiblePath p r w) -> (Q "path", [ ("path", JSON_Q $ Q p)
                                         , ("readable", JSON_UQ $ jsonBool r)
                                         , ("writable", JSON_UQ $ jsonBool w)
                                         ])
    (Systemd t n) -> (Q "systemd", [ ("unittype", JSON_Q $ Q $ unitType t)
                                   , ("unit", JSON_Q $ Q n)])

dataDBusDependency :: DBusDependency_ -> DependencyData
dataDBusDependency d =
  case d of
    (DBusIO i)         -> dataIODependency_ i
    (Bus b)            -> (Q "bus", [("busname", JSON_Q $ Q $ formatBusName b)])
    (Endpoint b o i m) -> let (mt, mn) = memberData m
      in (Q "endpoint", [ ("busname", JSON_Q $ Q $ formatBusName b)
                        , ("objectpath", JSON_Q $ Q $ formatObjectPath o)
                        , ("interface", JSON_Q $ Q $ formatInterfaceName i)
                        , ("membertype", JSON_Q $ Q mt)
                        , ("membername", JSON_Q $ Q mn)
                        ])
  where
    memberData (Method_ n)   = ("method", formatMemberName n)
    memberData (Signal_ n)   = ("signal", formatMemberName n)
    memberData (Property_ n) = ("property", n)

fromResult :: Result a -> (Bool, [JSONQuotable])
fromResult = second (fmap Q) . either (False,) (\(PostPass _ ws) -> (True, ws))

fromResult_ :: Result_ -> (Bool, [JSONQuotable])
fromResult_ = second (fmap Q) . either (False,) (True,)

--------------------------------------------------------------------------------
-- | JSON formatting
--
-- I could use Aeson...but I don't feel like it (too many intermediate types)

newtype JSONQuotable = Q String

newtype JSONUnquotable = UQ String

data JSONMixed = JSON_UQ JSONUnquotable | JSON_Q JSONQuotable

jsonAlways :: Maybe JSONUnquotable -> [JSONUnquotable] -> [JSONUnquotable]
  -> JSONUnquotable
jsonAlways = jsonFeature True

jsonSometimes :: Maybe JSONUnquotable -> [JSONUnquotable] -> [JSONUnquotable]
  -> JSONUnquotable
jsonSometimes = jsonFeature False

jsonFeature :: Bool -> Maybe JSONUnquotable -> [JSONUnquotable]
  -> [JSONUnquotable] -> JSONUnquotable
jsonFeature isalways success failed untested = jsonObject
  [ ("type", JSON_Q $ Q $ if isalways then "always" else "sometimes")
  , ("success", JSON_UQ $ fromMaybe (UQ "null") success)
  , ("failed", JSON_UQ $ jsonArray $ fmap JSON_UQ failed)
  , ("untested", JSON_UQ $ jsonArray $ fmap JSON_UQ untested)
  ]

jsonSubfeature :: JSONQuotable -> JSONUnquotable -> JSONUnquotable
jsonSubfeature n r = jsonObject
  [ ("name", JSON_Q n)
  , ("root", JSON_UQ r)
  ]

jsonIORoot :: JSONUnquotable -> JSONUnquotable
jsonIORoot = jsonRoot True

jsonDBusRoot :: JSONUnquotable -> JSONUnquotable
jsonDBusRoot = jsonRoot False

jsonRoot :: Bool -> JSONUnquotable -> JSONUnquotable
jsonRoot isIO tree = jsonObject
  [ ("type", JSON_Q $ Q $ if isIO then "io" else "dbus")
  , ("tree", JSON_UQ tree)
  ]

jsonLeafUntested :: JSONQuotable -> [(String, JSONMixed)] -> JSONUnquotable
jsonLeafUntested = jsonLeaf Nothing

jsonLeaf :: Maybe (Bool, [JSONQuotable]) -> JSONQuotable -> [(String, JSONMixed)]
  -> JSONUnquotable
jsonLeaf status deptype depdata = jsonObject
  [ ("type", JSON_Q deptype)
  , ("status", jsonMaybe (JSON_UQ . uncurry jsonStatus) status)
  , ("data", JSON_UQ $ jsonObject depdata)
  ]

jsonStatus :: Bool -> [JSONQuotable] -> JSONUnquotable
jsonStatus present messages = jsonObject
  [ ("present", JSON_UQ $ jsonBool present)
  , ("messages", JSON_UQ $ jsonArray $ fmap JSON_Q messages)
  ]

jsonAnd :: JSONUnquotable -> JSONUnquotable -> JSONUnquotable
jsonAnd = jsonBranch True

jsonOr :: JSONUnquotable -> JSONUnquotable -> JSONUnquotable
jsonOr = jsonBranch False

jsonBranch :: Bool -> JSONUnquotable -> JSONUnquotable -> JSONUnquotable
jsonBranch isAnd l r = jsonObject
  [ ("test", JSON_Q $ Q $ if isAnd then "and" else "or")
  , ("left", JSON_UQ l)
  , ("right",JSON_UQ r)
  ]

jsonMaybe :: (a -> JSONMixed) -> Maybe a -> JSONMixed
jsonMaybe = maybe (JSON_UQ $ UQ "null")

jsonBool :: Bool -> JSONUnquotable
jsonBool True  = UQ "true"
jsonBool False = UQ "false"

jsonArray :: [JSONMixed] -> JSONUnquotable
jsonArray = UQ . bracket . intercalate "," . fmap quoteMaybe

jsonObject :: [(String, JSONMixed)] -> JSONUnquotable
jsonObject = UQ . curly . intercalate ","
  . fmap (\(k, v) -> doubleQuote k ++ ":" ++ quoteMaybe v)

quoteMaybe :: JSONMixed -> String
quoteMaybe (JSON_Q (Q s))   = doubleQuote s
quoteMaybe (JSON_UQ (UQ s)) = s

bracket :: String -> String
bracket s = "[" ++ s ++ "]"

curly :: String -> String
curly s = "{" ++ s ++ "}"

--------------------------------------------------------------------------------
-- | Other random formatting

failedMsgs :: Bool -> [SubfeatureFail] -> IO [String]
failedMsgs err = fmap concat . mapM (failedMsg err)

failedMsg :: Bool -> SubfeatureFail -> IO [String]
failedMsg err Subfeature { sfData = d, sfName = n } = do
  mapM (fmtMsg err n) $ case d of (PostMissing e) -> [e]; (PostFail es) -> es

fmtMsg :: Bool -> String -> String -> IO String
fmtMsg err n msg = do
  let e = if err then "ERROR" else "WARNING"
  p <- getProgName
  return $ unwords [bracket p, bracket e, bracket n, msg]

