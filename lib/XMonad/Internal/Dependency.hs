{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  -- feature types
  ( Feature
  , Always(..)
  , Always_(..)
  , FallbackRoot(..)
  , FallbackStack(..)
  , Sometimes(..)
  , Sometimes_
  , AlwaysX
  , AlwaysIO
  , SometimesX
  , SometimesIO
  , PostPass(..)
  , Subfeature(..)
  , SubfeatureRoot
  , Msg(..)
  , LogLevel(..)

  -- dependency tree types
  , Root(..)
  , Tree(..)
  , Tree_(..)
  , IOTree
  , IOTree_
  , DBusTree
  , DBusTree_
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
  , FIO
  , withCache
  , evalFeature
  , executeSometimes
  , executeAlways
  , evalAlways
  , evalSometimes
  , fontTreeAlt
  , fontTree
  , fontTree_
  , fontAlways
  , fontSometimes
  , readEthernet
  , readWireless

  -- lifting
  , ioSometimes
  , ioAlways

  -- feature construction
  , always1
  , sometimes1
  , sometimesIO
  , sometimesIO_
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
  , toAnd_
  , toFallback
  , pathR
  , pathRW
  , pathW
  -- , sysTest
  , voidResult
  , voidRead

  -- misc
  , shellTest
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Aeson              hiding (Error, Result)
import           Data.Aeson.Key
import           Data.Bifunctor
import           Data.Either
import qualified Data.HashMap.Strict     as H
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Yaml

import           GHC.Generics            (Generic)

import           DBus                    hiding (typeOf)
import           DBus.Client
import           DBus.Internal
import qualified DBus.Introspection      as I

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO.Error
import           System.Posix.Files

import           XMonad.Core             (X, io)
import           XMonad.Internal.IO
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import           XMonad.Internal.Theme

--------------------------------------------------------------------------------
-- | Feature Evaluation
--
-- Here we attempt to build and return the monadic actions encoded by each
-- feature.

-- | Run feature evaluation(s) with the cache
-- Currently there is no easy way to not use this (oh well)
withCache :: FIO a -> IO a
withCache x = do
  p <- getParams
  evalStateT (runReaderT x p) emptyCache

-- | Execute an Always immediately
executeAlways :: Always (IO a) -> FIO a
executeAlways = io <=< evalAlways

-- | Execute a Sometimes immediately (or do nothing if failure)
executeSometimes :: Sometimes (IO a) -> FIO (Maybe a)
executeSometimes a = maybe (return Nothing) (io . fmap Just) =<< evalSometimes a

-- | Possibly return the action of an Always/Sometimes
evalFeature :: Feature a -> FIO (Maybe a)
evalFeature (Right a) = Just <$> evalAlways a
evalFeature (Left s)  = evalSometimes s

-- | Possibly return the action of a Sometimes
evalSometimes :: Sometimes a -> FIO (Maybe a)
evalSometimes x = either goFail goPass =<< evalSometimesMsg x
  where
    goPass (a, ws) = putErrors ws >> return (Just a)
    goFail es = putErrors es >> return Nothing
    putErrors = mapM_ printMsg

-- | Return the action of an Always
evalAlways :: Always a -> FIO a
evalAlways a = do
  (x, ws) <- evalAlwaysMsg a
  mapM_ printMsg ws
  return x

printMsg :: FMsg -> FIO ()
printMsg (FMsg fn n (Msg ll m)) = do
  xl <- asks xpLogLevel
  p <- io getProgName
  io $ when (ll >= xl) $ putStrLn $ unwords [ bracket p
                                            , bracket $ show ll
                                            , bracket fn
                                            , bracket n, m
                                            ]

--------------------------------------------------------------------------------
-- | Feature status

-- | Dump the status of a Feature
dumpFeature :: Feature a -> FIO JSONUnquotable
dumpFeature = either dumpSometimes dumpAlways

-- | Dump the status of an Always to stdout
dumpAlways :: Always a -> FIO JSONUnquotable
dumpAlways (Always n x) = go [] x
  where
    go failed (Option o os) = do
      (s, r) <- dumpSubfeatureRoot o
      if r
        then return $ jsonAlways (Q n) (Just s) failed $ untested [] os
        else go (s:failed) os
    go failed (Always_ _) = return $ jsonAlways (Q n) (Just (UQ "true")) failed []
    untested acc (Always_ _)   = acc
    untested acc (Option o os) = untested (dataSubfeatureRoot o:acc) os

-- | Dump the status of a Sometimes to stdout
dumpSometimes :: Sometimes a -> FIO JSONUnquotable
dumpSometimes (Sometimes n a) = go [] a
  where
    go failed [] = return $ jsonSometimes (Q n) Nothing failed []
    go failed (x:xs) = do
      (s, r) <- dumpSubfeatureRoot x
      if r
        then return $ jsonSometimes (Q n) (Just s) failed $ fmap dataSubfeatureRoot xs
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
data Always a = Always String (Always_ a)

-- | Feature that is guaranteed to work (inner data)
data Always_ a = Option (SubfeatureRoot a) (Always_ a)
  | Always_ (FallbackRoot a)

-- | Root of a fallback action for an always
-- This may either be a lone action or a function that depends on the results
-- from other Always features.
data FallbackRoot a = FallbackAlone a
  | forall p. FallbackTree (p -> a) (FallbackStack p)

-- | Always features that are used as a payload for a fallback action
data FallbackStack p = FallbackBottom (Always p)
  | forall x y. FallbackStack (x -> y -> p) (Always x) (FallbackStack y)

-- | Feature that might not be present
-- This is like an Always except it doesn't fall back on a guaranteed monadic
-- action
data Sometimes a = Sometimes String (Sometimes_ a)

-- | Feature that might not be present (inner data)
type Sometimes_ a = [SubfeatureRoot a]

-- | Individually tested sub-feature data for Always/sometimes
-- The polymorphism allows representing tested and untested states. Includes
-- the 'action' itself to be tested and any auxilary data for describing the
-- sub-feature.
data Subfeature f = Subfeature
  { sfData  :: f
  , sfName  :: String
  , sfLevel :: LogLevel
  }

-- | Loglevel at which feature testing should be reported
-- This is currently not used for anything important
data LogLevel = Silent | Error | Warn | Debug deriving (Eq, Show, Ord, Generic)

type SubfeatureRoot a = Subfeature (Root a)

-- | An action and its dependencies
-- May be a plain old monad or be DBus-dependent, in which case a client is
-- needed
data Root a = forall p. IORoot (p -> a) (IOTree p)
  | IORoot_ a IOTree_
  | forall p. DBusRoot (p -> Client -> a) (DBusTree p) (Maybe Client)
  | DBusRoot_ (Client -> a) DBusTree_ (Maybe Client)

-- | The dependency tree with rule to merge results when needed
data Tree d d_ p =
  forall x y. And12 (x -> y -> p) (Tree d d_ x) (Tree d d_ y)
  | And1 (Tree d d_ p) (Tree_ d_)
  | And2 (Tree_ d_) (Tree d d_ p)
  | Or (Tree d d_ p) (Tree d d_ p)
  | Only (d p)

-- | A dependency tree without functions to merge results
data Tree_ d = And_ (Tree_ d) (Tree_ d) | Or_ (Tree_ d) (Tree_ d) | Only_ d

-- | Shorthand tree types for lazy typers
type IOTree p = Tree IODependency IODependency_ p
type DBusTree p = Tree IODependency DBusDependency_ p
type IOTree_ = Tree_ IODependency_
type DBusTree_ = Tree_ DBusDependency_

-- | A dependency that only requires IO to evaluate (with payload)
data IODependency p =
  -- a cachable IO action that yields a payload
  IORead String (FIO (Result p))
  -- always yields a payload
  | IOConst p
  -- an always that yields a payload
  | forall a. IOAlways (Always a) (a -> p)
  -- a sometimes that yields a payload
  | forall a. IOSometimes (Sometimes a) (a -> p)

-- | A dependency pertaining to the DBus
data DBusDependency_ = Bus BusName
  | Endpoint BusName ObjectPath InterfaceName DBusMember
  | DBusIO IODependency_
  deriving (Eq, Generic)

instance Hashable DBusDependency_ where
  hashWithSalt s (Bus b)            = hashWithSalt s $ formatBusName b
  hashWithSalt s (Endpoint b o i m) = s `hashWithSalt` formatBusName b
                                      `hashWithSalt` formatObjectPath o
                                      `hashWithSalt` formatInterfaceName i
                                      `hashWithSalt` m
  hashWithSalt s (DBusIO i)            = hashWithSalt s i

-- | A dependency that only requires IO to evaluate (no payload)
data IODependency_ = IOSystem_ SystemDependency
  | IOTest_ String (IO (Maybe Msg))
  | forall a. IOSometimes_ (Sometimes a)

instance Eq IODependency_ where
  (==) (IOSystem_ s0) (IOSystem_ s1)     = s0 == s1
  (==) (IOTest_ _ _) (IOTest_ _ _)       = False
  (==) (IOSometimes_ _) (IOSometimes_ _) = False
  (==) _ _                               = False

instance Hashable IODependency_ where
  hashWithSalt s (IOSystem_ y)                  = hashWithSalt s y
  hashWithSalt s (IOTest_ n _)                  = hashWithSalt s n
  hashWithSalt s (IOSometimes_ (Sometimes n _)) = hashWithSalt s n

-- | A system component to an IODependency
-- This name is dumb, but most constructors should be obvious
data SystemDependency =
  Executable Bool FilePath
  | AccessiblePath FilePath Bool Bool
  | Systemd UnitType String
  deriving (Eq, Show, Generic)

instance Hashable SystemDependency

-- | The type of a systemd service
data UnitType = SystemUnit | UserUnit deriving (Eq, Show, Generic)

instance Hashable UnitType

-- | Wrapper type to describe and endpoint
data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show, Generic)

instance Hashable DBusMember where
  hashWithSalt s (Method_ m)   = hashWithSalt s $ formatMemberName m
  hashWithSalt s (Signal_ m)   = hashWithSalt s $ formatMemberName m
  hashWithSalt s (Property_ p) = hashWithSalt s p

--------------------------------------------------------------------------------
-- | Tested dependency tree
--
-- The main reason I need this is so I have a "result" I can convert to JSON
-- and dump on the CLI (unless there is a way to make Aeson work inside an IO)

-- | A message with criteria for when to show it
data Msg = Msg LogLevel String

-- | A message annotated with subfeature and feature name
data FMsg = FMsg String String Msg

-- | Tested Always feature
data PostAlways a = Primary (SubfeaturePass a) [SubfeatureFail] (Always_ a)
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
data PostPass a = PostPass a [Msg] deriving (Functor)

addMsgs :: PostPass a -> [Msg] -> PostPass a
addMsgs (PostPass a ms) ms' = PostPass a $ ms ++ ms'

-- | An action that failed
data PostFail = PostFail [Msg] | PostMissing Msg

--------------------------------------------------------------------------------
-- | Evaluation cache
--
-- Setting up trees like this usually entails having some repeated dependencies.
-- Testing the same dependency multiple times is stupid, so cache the results.
-- Note this is basically memorization, except that the results are IO-dependent
-- and this may technically change with each invocation. The assumption here is
-- that each repeated test without caching would be run in such close succession
-- that the results will always be the same.

emptyCache :: Cache
emptyCache = Cache H.empty H.empty H.empty

memoizeIO_ :: (IODependency_ -> FIO Result_) -> IODependency_ -> FIO Result_
memoizeIO_ f d = do
  m <- gets cIO_
  case H.lookup d m of
    (Just r) -> return r
    Nothing  -> do
      -- io $ putStrLn $ "not using cache for " ++ show d
      r <- f d
      modify (\s -> s { cIO_ = H.insert d r (cIO_ s) })
      return r

memoizeDBus_ :: (DBusDependency_ -> FIO Result_) -> DBusDependency_ -> FIO Result_
memoizeDBus_ f d = do
  m <- gets cDBus_
  case H.lookup d m of
    (Just r) -> return r
    Nothing  -> do
      -- io $ putStrLn $ "not using cache for " ++ show d
      r <- f d
      modify (\s -> s { cDBus_ = H.insert d r (cDBus_ s) })
      return r

memoizeFont :: (String -> IO (Result FontBuilder)) -> String -> FIO (Result FontBuilder)
memoizeFont f d = do
  m <- gets cFont
  case H.lookup d m of
    (Just r) -> return r
    Nothing  -> do
      -- io $ putStrLn $ "not using cache for " ++ show d
      r <- io $ f d
      modify (\s -> s { cFont = H.insert d r (cFont s) })
      return r

--------------------------------------------------------------------------------
-- | Configuration

type FIO a = ReaderT XParams (StateT Cache IO) a

data XParams = XParams
  { xpLogLevel :: LogLevel
  }

defParams :: XParams
defParams = XParams { xpLogLevel = Error }

data Cache = Cache
  { --cIO    :: forall p. Memoizable p => H.HashMap (IODependency p) (Result p)
    cIO_   :: H.HashMap IODependency_ Result_
  , cDBus_ :: H.HashMap DBusDependency_ Result_
  , cFont  :: H.HashMap String (Result FontBuilder)
  }

getParams :: IO XParams
getParams = do
  p <- getParamFile
  maybe (return defParams) decodeYaml p
  where
    decodeYaml p = either (\e -> print e >> return defParams) return
      =<< decodeFileEither p

getParamFile :: IO (Maybe FilePath)
getParamFile = do
  e <- lookupEnv "XDG_CONFIG_HOME"
  parent <- case e of
    Nothing -> fallback
    Just path
      | isRelative path -> fallback
      | otherwise -> return path
  let full = parent </> "xmonad.yml"
  (\x -> if x then Just full else Nothing) <$> fileExist full
  where
    fallback = (</> ".config") <$> getHomeDirectory

instance FromJSON XParams where
  parseJSON = withObject "parameters" $ \o -> XParams
    <$> o .: fromString "loglevel"

instance FromJSON LogLevel

--------------------------------------------------------------------------------
-- | Testing pipeline

evalSometimesMsg :: Sometimes a -> FIO (Either [FMsg] (a, [FMsg]))
evalSometimesMsg (Sometimes n xs) = do
  PostSometimes { psSuccess = s, psFailed = fs } <- testSometimes xs
  let fs' = failedMsgs n fs
  return $ case s of
    (Just p) -> Right $ second (++ fs') $ passActMsg n p
    _        -> Left fs'

evalAlwaysMsg :: Always a -> FIO (a, [FMsg])
evalAlwaysMsg (Always n x) = do
  r <- testAlways x
  return $ case r of
    (Primary p fs _)  -> second (++ failedMsgs n fs) $ passActMsg n p
    (Fallback act fs) -> (act, failedMsgs n fs)

passActMsg :: String -> SubfeaturePass a -> (a, [FMsg])
passActMsg fn Subfeature { sfData = PostPass a ws, sfName = n } = (a, fmap (FMsg fn n) ws)

failedMsgs :: String -> [SubfeatureFail] -> [FMsg]
failedMsgs n = concatMap (failedMsg n)

failedMsg :: String -> SubfeatureFail -> [FMsg]
failedMsg fn Subfeature { sfData = d, sfName = n } = case d of
  (PostFail es)   -> f es
  (PostMissing e) -> f [e]
  where
    f = fmap (FMsg fn n)

testAlways :: Always_ a -> FIO (PostAlways a)
testAlways = go []
  where
    go failed (Option fd next) = do
      r <- testSubfeature fd
      case r of
        (Left l)     -> go (l:failed) next
        (Right pass) -> return $ Primary pass failed next
    go failed (Always_ ar) = (`Fallback` failed) <$> evalFallbackRoot ar

evalFallbackRoot :: FallbackRoot a -> FIO a
evalFallbackRoot (FallbackAlone a)  = return a
evalFallbackRoot (FallbackTree a s) = a <$> evalFallbackStack s

evalFallbackStack :: FallbackStack p -> FIO p
evalFallbackStack (FallbackBottom a) = evalAlways a
evalFallbackStack (FallbackStack f a as) = do
  ps <- evalFallbackStack as
  p <- evalAlways a
  return $ f p ps

testSometimes :: Sometimes_ a -> FIO (PostSometimes a)
testSometimes = go (PostSometimes Nothing [])
  where
    go ts [] = return ts
    go ts (x:xs) = do
      sf <- testSubfeature x
      case sf of
        (Left l)     -> go (ts { psFailed = l:psFailed ts }) xs
        (Right pass) -> return $ ts { psSuccess = Just pass }

testSubfeature :: SubfeatureRoot a -> FIO (Either SubfeatureFail (SubfeaturePass a))
testSubfeature sf@Subfeature{ sfData = t } = do
  t' <- testRoot t
  -- monomorphism restriction :(
  return $ bimap (\n -> sf { sfData = n }) (\n -> sf { sfData = n }) t'

testRoot :: Root a -> FIO (Either PostFail (PostPass a))
testRoot r = do
  case r of
    (IORoot a t)              -> go a testIODependency_ testIODependency t
    (IORoot_ a t)             -> go_ a testIODependency_ t
    (DBusRoot a t (Just cl))  -> go (`a` cl) (testDBusDependency_ cl) testIODependency t
    (DBusRoot_ a t (Just cl)) -> go_ (a cl) (testDBusDependency_ cl) t
    _                         -> return $ Left $ PostMissing
                                 $ Msg Error "client not available"
  where
    -- rank N polymorphism is apparently undecidable...gross
    go a f_ (f :: forall q. d q -> FIO (Result q)) t =
      bimap PostFail (fmap a) <$> testTree f_ f t
    go_ a f_ t = bimap PostFail (PostPass a) <$> testTree_ f_ t

--------------------------------------------------------------------------------
-- | Payloaded dependency testing

type Result p = Either [Msg] (PostPass p)

testTree :: forall d d_ p. (d_ -> FIO Result_) -> (forall q. d q -> FIO (Result q))
  -> Tree d d_ p -> FIO (Either [Msg] (PostPass p))
testTree test_ test = go
  where
    go :: forall q. Tree d d_ q -> FIO (Either [Msg] (PostPass q))
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

testIODependency :: IODependency p -> FIO (Result p)
testIODependency (IORead _ t)      = t
testIODependency (IOConst c)       = return $ Right $ PostPass c []
-- TODO this is a bit odd because this is a dependency that will always
-- succeed, which kinda makes this pointless. The only reason I would want this
-- is if I want to have a built-in logic to "choose" a payload to use in
-- building a higher-level feature
testIODependency (IOAlways a f)    = Right . uncurry PostPass
  -- TODO this is wetter than Taco Bell shit
  . bimap f (fmap stripMsg) <$> evalAlwaysMsg a
testIODependency (IOSometimes x f) =
  bimap (fmap stripMsg) (uncurry PostPass . bimap f (fmap stripMsg))
  <$> evalSometimesMsg x

stripMsg :: FMsg -> Msg
stripMsg (FMsg _ _ m) = m

--------------------------------------------------------------------------------
-- | Standalone dependency testing

type Result_ = Either [Msg] [Msg]

testTree_ :: (d -> FIO Result_) -> Tree_ d -> FIO Result_
testTree_ test = go
  where
    go (And_ a b) = either (return . Left) (`test2nd` b) =<< go a
    go (Or_ a b)  = either (`test2nd` b) (return . Right) =<< go a
    go (Only_ a)  = test a
    test2nd ws = fmap ((Right . (ws ++)) =<<) . go

testIODependency_ :: IODependency_ -> FIO Result_
testIODependency_ = memoizeIO_ testIODependency'_

testIODependency'_ :: IODependency_ -> FIO Result_
testIODependency'_ (IOSystem_ s) = io $ readResult_ <$> testSysDependency s
testIODependency'_ (IOTest_ _ t) = io $ readResult_ <$> t
testIODependency'_ (IOSometimes_ x) = bimap (fmap stripMsg) (fmap stripMsg . snd)
  <$> evalSometimesMsg x

--------------------------------------------------------------------------------
-- | System Dependency Testing

testSysDependency :: SystemDependency -> IO (Maybe Msg)
testSysDependency (Executable sys bin) = maybe (Just msg) (const Nothing)
  <$> findExecutable bin
  where
    msg = Msg Error $ unwords [e, "executable", singleQuote bin, "not found"]
    e = if sys then "system" else "local"
testSysDependency (Systemd t n) = shellTest cmd msg
  where
    msg = unwords ["systemd", unitType t, "unit", singleQuote n, "not found"]
    cmd = fmtCmd "systemctl" $ ["--user" | t == UserUnit] ++ ["status", n]
testSysDependency (AccessiblePath p r w) = permMsg <$> getPermissionsSafe p
  where
    testPerm False _ _  = Nothing
    testPerm True f res = Just $ f res
    mkErr = Just . Msg Error
    permMsg NotFoundError            = mkErr "file not found"
    permMsg PermError                = mkErr "could not get permissions"
    permMsg (PermResult res) =
      case (testPerm r readable res, testPerm w writable res) of
        (Just False, Just False) -> mkErr "file not readable or writable"
        (Just False, _)          -> mkErr "file not readable"
        (_, Just False)          -> mkErr "file not writable"
        _                        -> Nothing

shellTest :: String -> String -> IO (Maybe Msg)
shellTest cmd msg = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> Nothing
    _           -> Just $ Msg Error msg

unitType :: UnitType -> String
unitType SystemUnit = "system"
unitType UserUnit   = "user"

--------------------------------------------------------------------------------
-- | IO testers
--
-- Make a special case for these since we end up testing the font alot, and it
-- would be nice if I can cache them.

fontAlways :: String -> String -> Always FontBuilder
fontAlways n fam = always1 n (fontFeatureName fam) root fallbackFont
  where
    root = IORoot id $ fontTree fam

fontSometimes :: String -> String -> Sometimes FontBuilder
fontSometimes n fam = sometimes1 n (fontFeatureName fam) root
  where
    root = IORoot id $ fontTree fam

fontFeatureName :: String -> String
fontFeatureName n = unwords ["Font family for", singleQuote n]

fontTreeAlt :: String -> Tree IODependency d_ FontBuilder
fontTreeAlt fam = Or (fontTree fam) $ Only $ IOConst fallbackFont

fontTree :: String -> Tree IODependency d_ FontBuilder
fontTree = Only . fontDependency

fontTree_ :: String -> IOTree_
fontTree_ = Only_ . fontDependency_

fontDependency :: String -> IODependency FontBuilder
fontDependency fam = IORead (fontTestName fam) $ testFont fam

fontDependency_ :: String -> IODependency_
fontDependency_ fam = IOTest_ (fontTestName fam) $ voidRead <$> testFont' fam

fontTestName :: String -> String
fontTestName fam = unwords ["test if font", singleQuote fam, "exists"]

testFont :: String -> FIO (Result FontBuilder)
testFont = memoizeFont testFont'

testFont' :: String -> IO (Result FontBuilder)
testFont' fam = maybe pass (Left . (:[])) <$> shellTest cmd msg
  where
    msg = unwords ["font family", qFam, "not found"]
    cmd = fmtCmd "fc-list" ["-q", qFam]
    qFam = singleQuote fam
    pass = Right $ PostPass (buildFont $ Just fam) []

--------------------------------------------------------------------------------
-- | network dependencies
--
-- ASSUME that the system uses systemd in which case ethernet interfaces always
-- start with "en" and wireless interfaces always start with "wl"

readEthernet :: IODependency String
readEthernet = readInterface "get ethernet interface" isEthernet

readWireless :: IODependency String
readWireless = readInterface "get wireless interface" isWireless

isWireless :: String -> Bool
isWireless ('w':'l':_) = True
isWireless _           = False

isEthernet :: String -> Bool
isEthernet ('e':'n':_) = True
isEthernet _           = False

listInterfaces :: IO [String]
listInterfaces = fromRight [] <$> tryIOError (listDirectory sysfsNet)

sysfsNet :: FilePath
sysfsNet = "/sys/class/net"

readInterface :: String -> (String -> Bool) -> IODependency String
readInterface n f = IORead n go
  where
    go = io $ do
      ns <- filter f <$> listInterfaces
      case ns of
        [] -> return $ Left [Msg Error "no interfaces found"]
        (x:xs) -> do
          return $ Right $ PostPass x
            $ fmap (Msg Warn . ("ignoring extra interface: " ++)) xs

--------------------------------------------------------------------------------
-- | DBus Dependency Testing

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

testDBusDependency_ :: Client -> DBusDependency_ -> FIO Result_
testDBusDependency_ cl = memoizeDBus_ (testDBusDependency'_ cl)

testDBusDependency'_ :: Client -> DBusDependency_ -> FIO Result_
testDBusDependency'_ client (Bus bus) = io $ do
  ret <- callMethod client queryBus queryPath queryIface queryMem
  return $ case ret of
        Left e    -> Left [Msg Error e]
        Right b -> let ns = bodyGetNames b in
          if bus' `elem` ns then Right []
          else Left [
            Msg Error $ unwords ["name", singleQuote bus', "not found on dbus"]
            ]
  where
    bus' = formatBusName bus
    queryBus = busName_ "org.freedesktop.DBus"
    queryIface = interfaceName_ "org.freedesktop.DBus"
    queryPath = objectPath_ "/"
    queryMem = memberName_ "ListNames"
    bodyGetNames [v] = fromMaybe [] $ fromVariant v :: [String]
    bodyGetNames _   = []

testDBusDependency'_ client (Endpoint busname objpath iface mem) = io $ do
  ret <- callMethod client busname objpath introspectInterface introspectMethod
  return $ case ret of
        Left e     -> Left [Msg Error e]
        Right body -> procBody body
  where
    procBody body = let res = findMem =<< I.parseXML objpath =<< fromVariant
                          =<< listToMaybe body in
      case res of
        Just True -> Right []
        _         -> Left [Msg Error $ fmtMsg' mem]
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

testDBusDependency'_ _ (DBusIO i) = testIODependency_ i

--------------------------------------------------------------------------------
-- | IO Lifting functions

ioSometimes :: MonadIO m => Sometimes (IO a) -> Sometimes (m a)
ioSometimes (Sometimes n xs) = Sometimes n $ fmap ioSubfeature xs

ioAlways :: MonadIO m => Always (IO a) -> Always (m a)
ioAlways (Always n x)   = Always n $ ioAlways' x

ioAlways' :: MonadIO m => Always_ (IO a) -> Always_ (m a)
ioAlways' (Always_ ar)  = Always_ $ ioFallbackRoot ar
ioAlways' (Option sf a) = Option (ioSubfeature sf) $ ioAlways' a

ioFallbackRoot :: MonadIO m => FallbackRoot (IO a) -> FallbackRoot (m a)
ioFallbackRoot (FallbackAlone a)  = FallbackAlone $ io a
ioFallbackRoot (FallbackTree a s) = FallbackTree (io . a) s

ioSubfeature :: MonadIO m => SubfeatureRoot (IO a) -> SubfeatureRoot (m a)
ioSubfeature sf = sf { sfData = ioRoot $ sfData sf }

ioRoot :: MonadIO m => Root (IO a) -> Root (m a)
ioRoot (IORoot a t)       = IORoot (io . a) t
ioRoot (IORoot_ a t)      = IORoot_ (io a) t
ioRoot (DBusRoot a t cl)  = DBusRoot (\p c -> io $ a p c) t cl
ioRoot (DBusRoot_ a t cl) = DBusRoot_ (io . a) t cl

--------------------------------------------------------------------------------
-- | Feature constructors

sometimes1_ :: LogLevel -> String -> String -> Root a -> Sometimes a
sometimes1_ l fn n t = Sometimes fn
  [Subfeature{ sfData = t, sfName = n, sfLevel = l }]

always1_ :: LogLevel -> String -> String -> Root a -> a -> Always a
always1_ l fn n t x = Always fn
  $ Option (Subfeature{ sfData = t, sfName = n, sfLevel = l }) (Always_ $ FallbackAlone x)

sometimes1 :: String -> String -> Root a -> Sometimes a
sometimes1 = sometimes1_ Error

always1 :: String -> String -> Root a -> a -> Always a
always1 = always1_ Error

sometimesIO_ :: String -> String -> IOTree_ -> a -> Sometimes a
sometimesIO_ fn n t x = sometimes1 fn n $ IORoot_ x t

sometimesIO :: String -> String -> IOTree p -> (p -> a) -> Sometimes a
sometimesIO fn n t x = sometimes1 fn n $ IORoot x t

sometimesExe :: MonadIO m => String -> String -> Bool -> FilePath -> Sometimes (m ())
sometimesExe fn n sys path = sometimesExeArgs fn n sys path []

sometimesExeArgs :: MonadIO m => String -> String -> Bool -> FilePath
  -> [String] -> Sometimes (m ())
sometimesExeArgs fn n sys path args =
  sometimesIO_ fn n (Only_ (IOSystem_ $ Executable sys path)) $ spawnCmd path args

sometimesDBus :: Maybe Client -> String -> String -> Tree_ DBusDependency_
  -> (Client -> a) -> Sometimes a
sometimesDBus c fn n t x = sometimes1 fn n $ DBusRoot_ x t c

sometimesEndpoint :: MonadIO m => String -> String -> BusName -> ObjectPath -> InterfaceName
  -> MemberName -> Maybe Client -> Sometimes (m ())
sometimesEndpoint fn name busname path iface mem client =
  sometimesDBus client fn name deps cmd
  where
    deps = Only_ $ Endpoint busname path iface $ Method_ mem
    cmd c = io $ void $ callMethod c busname path iface mem

--------------------------------------------------------------------------------
-- | Dependency Tree Constructors

listToAnds :: d -> [d] -> Tree_ d
listToAnds i = foldr (And_ . Only_) (Only_ i)

toAnd_ :: d -> d -> Tree_ d
toAnd_ a b = And_ (Only_ a) (Only_ b)

toFallback :: IODependency p -> p -> Tree IODependency d_ p
toFallback a = Or (Only a) . Only . IOConst

voidResult :: Result p -> Result_
voidResult (Left es)               = Left es
voidResult (Right (PostPass _ ws)) = Right ws

voidRead :: Result p -> Maybe Msg
voidRead (Left [])    = Just $ Msg Error "unspecified error"
voidRead (Left (e:_)) = Just e
voidRead (Right _)    = Nothing

readResult_ :: Maybe Msg -> Result_
readResult_ (Just w) = Left [w]
readResult_ _        = Right []

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

-- sysTest :: String -> IO (Maybe String) -> IODependency_
-- sysTest n = IOSystem_ . IOTest n

--------------------------------------------------------------------------------
-- | Printing

dumpSubfeatureRoot :: SubfeatureRoot a -> FIO (JSONUnquotable, Bool)
dumpSubfeatureRoot Subfeature { sfData = r, sfName = n } =
  first (jsonSubfeature $ Q n) <$> dumpRoot r

dumpRoot :: Root a -> FIO (JSONUnquotable, Bool)
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

dumpTree :: forall d d_ p. (forall q. d q -> FIO (Result q))
  -> (d_ -> FIO Result_) -> (forall q. d q -> DependencyData)
  -> (d_ -> DependencyData) -> Tree d d_ p -> FIO (JSONUnquotable, Bool)
dumpTree test test_ dd dd_ = go
  where
    go :: forall q. Tree d d_ q -> FIO (JSONUnquotable, Bool)
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
    data' :: forall q. Tree d d_ q -> JSONUnquotable
    data' = dataTree dd dd_
    dump_' = dumpTree_ test_ dd_
    doAnd fa fb fb_ a b = do
      (sa, ra) <- fa a
      let j = jsonAnd sa
      if ra then first j <$> fb b else return (j $ fb_ b, ra)

dumpTree_ :: (d_ -> FIO Result_) -> (d_ -> DependencyData) -> Tree_ d_
  -> FIO (JSONUnquotable, Bool)
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

dataTree :: forall d d_ p. (forall q. d q -> DependencyData)
  -> (d_ -> DependencyData) -> Tree d d_ p -> JSONUnquotable
dataTree f f_ = go
  where
    go :: forall q. Tree d d_ q -> JSONUnquotable
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
dataIODependency d = first Q $ case d of
  (IORead n _)                    -> ("ioread", [("desc", JSON_Q $ Q n)])
  (IOConst _)                     -> ("const", [])
  (IOSometimes (Sometimes n _) _) -> ("sometimes", [("name", JSON_Q $ Q n)])
  (IOAlways (Always n _) _)       -> ("always", [("name", JSON_Q $ Q n)])

dataIODependency_ :: IODependency_ -> DependencyData
dataIODependency_ d = case d of
  (IOSystem_ s)    -> dataSysDependency s
  (IOSometimes_ _) -> (Q "sometimes", [])
  (IOTest_ desc _) -> (Q "iotest", [("desc", JSON_Q $ Q desc)])

dataSysDependency :: SystemDependency -> DependencyData
dataSysDependency d = first Q $
  case d of
    (Executable sys path) -> ("executable", [ ("system", JSON_UQ $ jsonBool sys)
                                            , ("path", JSON_Q $ Q path)
                                            ])
    (AccessiblePath p r w) -> ("path", [ ("path", JSON_Q $ Q p)
                                       , ("readable", JSON_UQ $ jsonBool r)
                                       , ("writable", JSON_UQ $ jsonBool w)
                                       ])
    (Systemd t n) -> ("systemd", [ ("unittype", JSON_Q $ Q $ unitType t)
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

fromMsg :: Msg -> JSONUnquotable
fromMsg (Msg e s) = jsonObject [ ("level", JSON_Q $ Q $ show e)
                               , ("msg", JSON_Q $ Q s)
                               ]

fromResult :: Result a -> (Bool, [JSONUnquotable])
fromResult = second (fmap fromMsg) . either (False,) (\(PostPass _ ws) -> (True, ws))

fromResult_ :: Result_ -> (Bool, [JSONUnquotable])
fromResult_ = second (fmap fromMsg) . either (False,) (True,)

--------------------------------------------------------------------------------
-- | JSON formatting
--
-- I could use Aeson...but I don't feel like it (too many intermediate types)

newtype JSONQuotable = Q String

newtype JSONUnquotable = UQ String

data JSONMixed = JSON_UQ JSONUnquotable | JSON_Q JSONQuotable

jsonAlways :: JSONQuotable -> Maybe JSONUnquotable -> [JSONUnquotable]
  -> [JSONUnquotable] -> JSONUnquotable
jsonAlways = jsonFeature True

jsonSometimes :: JSONQuotable -> Maybe JSONUnquotable -> [JSONUnquotable]
  -> [JSONUnquotable] -> JSONUnquotable
jsonSometimes = jsonFeature False

jsonFeature :: Bool -> JSONQuotable -> Maybe JSONUnquotable -> [JSONUnquotable]
  -> [JSONUnquotable] -> JSONUnquotable
jsonFeature isalways name success failed untested = jsonObject
  [ ("type", JSON_Q $ Q $ if isalways then "always" else "sometimes")
  , ("name", JSON_Q name)
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

jsonLeaf :: Maybe (Bool, [JSONUnquotable]) -> JSONQuotable -> [(String, JSONMixed)]
  -> JSONUnquotable
jsonLeaf status deptype depdata = jsonObject
  [ ("type", JSON_Q deptype)
  , ("status", jsonMaybe (JSON_UQ . uncurry jsonStatus) status)
  , ("data", JSON_UQ $ jsonObject depdata)
  ]

jsonStatus :: Bool -> [JSONUnquotable] -> JSONUnquotable
jsonStatus present messages = jsonObject
  [ ("present", JSON_UQ $ jsonBool present)
  , ("messages", JSON_UQ $ jsonArray $ fmap JSON_UQ messages)
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

-- failedMsgsIO :: Bool -> String -> [SubfeatureFail] -> FIO [Msg]
-- failedMsgsIO err fn = io . failedMsgs err fn

-- failedMsgs :: Bool -> String -> [SubfeatureFail] -> IO [Msg]
-- failedMsgs err fn = fmap concat . mapM (failedMsg err fn)

-- failedMsg :: Bool -> String -> SubfeatureFail -> IO [Msg]
-- failedMsg err fn Subfeature { sfData = d, sfName = n } = do
--   mapM (fmtMsg err fn n) $ case d of (PostMissing e) -> [e]; (PostFail es) -> es

-- fmtMsg :: Bool -> String -> String -> Msg -> IO Msg
-- fmtMsg err fn n msg = do
--   let e = if err then "ERROR" else "WARNING"
--   p <- getProgName
--   return $ unwords [bracket p, bracket e, bracket fn, bracket n, msg]

