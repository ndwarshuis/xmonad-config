{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( AlwaysX
  , AlwaysIO
  , Always(..)
  , SometimesX
  , SometimesIO
  , Sometimes
  , executeSometimes_
  , executeSometimes
  , executeAlways_
  , executeAlways
  , evalAlways
  , evalSometimes

  , Subfeature(..)
  , LogLevel(..)

  , Action(..)

  -- feature construction
  , sometimes1
  , sometimesIO
  , sometimesDBus
  , sometimesExe
  , sometimesExeArgs
  , sometimesEndpoint

  -- Dependency tree
  , ActionTree(..)
  , Tree(..)
  , IODependency(..)
  , DBusDependency(..)
  , DBusMember(..)
  , UnitType(..)
  , listToAnds
  , toAnd
  , pathR
  , pathRW
  , pathW
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

-- data AlwaysAny = AX AlwaysX | AIO AlwaysIO

type AlwaysX = Always (X ())

type AlwaysIO = Always (IO ())

type SometimesX = Sometimes (X ())

type SometimesIO = Sometimes (IO ())

data Always a = Option (Subfeature a Tree) (Always a) | Always a

type Sometimes a = [Subfeature a Tree]

data TestedAlways a p =
  Primary (Finished a p) [FailedFeature a p] (Always a)
  | Fallback a [FailedFeature a p]

data TestedSometimes a p = TestedSometimes
  { tsSuccess  :: Maybe (Finished a p)
  , tsFailed   :: [FailedFeature a p]
  , tsUntested :: [Subfeature a Tree]
  }

type FailedFeature a p = Either (Subfeature a Tree, String)
  (Subfeature a ResultTree, [String])

data Finished a p = Finished
  { finData     :: Subfeature a ResultTree
  , finAction   :: a
  , finWarnings :: [String]
  }

data FeatureResult a p = Untestable (Subfeature a Tree) String |
  FailedFtr (Subfeature a ResultTree) [String] |
  SuccessfulFtr (Finished a p)

type ActionTreeMaybe a p = Either (ActionTree a Tree, String)
  (ActionTree a ResultTree, Maybe a, [String])

sometimes1_ :: LogLevel -> String -> ActionTree a Tree -> Sometimes a
sometimes1_ l n t = [Subfeature{ sfTree = t, sfName = n, sfLevel = l }]

always1_ :: LogLevel -> String -> ActionTree a Tree -> a -> Always a
always1_ l n t x =
  Option (Subfeature{ sfTree = t, sfName = n, sfLevel = l }) (Always x)

sometimes1 :: String -> ActionTree a Tree -> Sometimes a
sometimes1 = sometimes1_ Error

sometimesIO :: String -> Tree (IODependency a p) p -> a -> Sometimes a
sometimesIO n t x = sometimes1 n $ IOTree (Standalone x) t

sometimesDBus :: Maybe Client -> String -> Tree (DBusDependency a p) p
  -> (Client -> a) -> Sometimes a
sometimesDBus c n t x = sometimes1 n $ DBusTree (Standalone x) c t

--------------------------------------------------------------------------------
-- | Feature Data

data Subfeature a t = Subfeature
  { sfTree  :: ActionTree a t
  , sfName  :: String
  , sfLevel :: LogLevel
  }

data LogLevel = Silent | Error | Warn | Debug deriving (Eq, Show, Ord)

data Msg = Msg LogLevel String String

--------------------------------------------------------------------------------
-- | Action Tree

data ActionTree a t =
  forall p. IOTree (Action a p) (t (IODependency a p) p)
  | forall p. DBusTree (Action (Client -> a) p) (Maybe Client) (t (DBusDependency a p) p)

data Action a p = Standalone a | Consumer (p -> a)

--------------------------------------------------------------------------------
-- | Dependency Tree

data Tree d p =
  And (p -> p -> p) (Tree d p) (Tree d p)
  | Or (p -> p) (p -> p) (Tree d p) (Tree d p)
  | Only d

listToAnds :: d -> [d] -> Tree d (Maybe x)
listToAnds i = foldr (And (const . const Nothing) . Only) (Only i)

toAnd :: d -> d -> Tree d (Maybe x)
toAnd a b = And (const . const Nothing) (Only a) (Only b)

--------------------------------------------------------------------------------
-- | Result Tree

-- | how to interpret ResultTree combinations:
-- First (LeafSuccess a) (Tree a) -> Or that succeeded on left
-- First (LeafFail a) (Tree a) -> And that failed on left
-- Both (LeafFail a) (Fail a) -> Or that failed
-- Both (LeafSuccess a) (LeafSuccess a) -> And that succeeded
-- Both (LeafFail a) (LeafSuccess a) -> Or that failed first and succeeded second
-- Both (LeafSuccess a) (LeafFail a) -> And that failed on the right

data ResultTree d p =
  First (ResultTree d p) (Tree d p)
  | Both  (ResultTree d p) (ResultTree d p)
  | LeafSuccess d [String]
  | LeafFail d [String]

type Payload p = (Maybe p, [String])

type Summary p = Either [String] (Payload p)

smryNil :: q -> Summary p
smryNil = const $ Right (Nothing, [])

smryFail :: String -> Either [String] a
smryFail msg = Left [msg]

smryInit :: Summary p
smryInit = Right (Nothing, [])

foldResultTreeMsgs :: ResultTree d p -> ([String], [String])
foldResultTreeMsgs = undefined

--------------------------------------------------------------------------------
-- | Result

type Result p = Either [String] (Maybe p)

resultNil :: p -> Result q
resultNil = const $ Right Nothing

--------------------------------------------------------------------------------
-- | IO Dependency

data IODependency a p = Executable Bool FilePath
  | AccessiblePath FilePath Bool Bool
  | IOTest String (IO (Maybe String))
  | IORead String (IO (Either String (Maybe p)))
  | Systemd UnitType String
  | NestedAlways (Always a) (a -> p)
  | NestedSometimes (Sometimes a) (a -> p)

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

sometimesExe :: MonadIO m => String -> Bool -> FilePath -> Sometimes (m ())
sometimesExe n sys path = sometimesExeArgs n sys path []

sometimesExeArgs :: MonadIO m => String -> Bool -> FilePath -> [String] -> Sometimes (m ())
sometimesExeArgs n sys path args =
  sometimesIO n (Only (Executable sys path)) $ spawnCmd path args

pathR :: String -> IODependency a p
pathR n = AccessiblePath n True False

pathW :: String -> IODependency a p
pathW n = AccessiblePath n False True

pathRW :: String -> IODependency a p
pathRW n = AccessiblePath n True True

--------------------------------------------------------------------------------
-- | DBus Dependency Result

data DBusDependency a p =
  Bus BusName
  | Endpoint BusName ObjectPath InterfaceName DBusMember
  | DBusIO (IODependency a p)

data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

sometimesEndpoint :: MonadIO m => String -> BusName -> ObjectPath -> InterfaceName
  -> MemberName -> Maybe Client -> Sometimes (m ())
sometimesEndpoint name busname path iface mem client =
  sometimesDBus client name deps cmd
  where
    deps = Only $ Endpoint busname path iface $ Method_ mem
    cmd c = io $ void $ callMethod c busname path iface mem

--------------------------------------------------------------------------------
-- | Feature evaluation
--
-- Here we attempt to build and return the monadic actions encoded by each
-- feature.

executeSometimes_ :: MonadIO m => Sometimes (m a) -> m ()
executeSometimes_ = void . executeSometimes

executeSometimes :: MonadIO m => Sometimes (m a) -> m (Maybe a)
executeSometimes a = maybe (return Nothing) (fmap Just) =<< evalSometimes a

-- TODO actually print things
evalSometimes :: MonadIO m => Sometimes a -> m (Maybe a)
evalSometimes x = either (const Nothing) (Just . fst) <$> evalSometimesMsg x

-- TODO actually collect error messages here
-- TODO add feature name to errors
evalSometimesMsg :: MonadIO m => Sometimes a -> m (Either [String] (a, [String]))
evalSometimesMsg x = io $ do
  TestedSometimes { tsSuccess = s, tsFailed = _ } <- testSometimes x
  return $ maybe (Left []) (\Finished { finAction = a } -> Right (a, [])) s

executeAlways_ :: MonadIO m => Always (m a) -> m ()
executeAlways_ = void . executeAlways

executeAlways :: MonadIO m => Always (m a) -> m a
executeAlways = join . evalAlways

-- TODO actually print things
evalAlways :: MonadIO m => Always a -> m a
evalAlways a = fst <$> evalAlwaysMsg a

evalAlwaysMsg :: MonadIO m => Always a -> m (a, [String])
evalAlwaysMsg a = io $ do
  r <- testAlways a
  return $ case r of
    (Primary (Finished { finAction = act }) _ _) -> (act, [])
    (Fallback act _)                             -> (act, [])

--------------------------------------------------------------------------------
-- | Dependency Testing
--
-- Here we test all dependencies and keep the tree structure so we can print it
-- for diagnostic purposes. This obviously has overlap with feature evaluation
-- since we need to resolve dependencies to build each feature.

testAlways :: Always a -> IO (TestedAlways a p)
testAlways = go []
  where
    go failed (Option fd next) = do
      r <- testSubfeature fd
      case r of
        (Untestable fd' err) -> go (Left (fd' ,err):failed) next
        (FailedFtr fd' errs) -> go (Right (fd' ,errs):failed) next
        (SuccessfulFtr s)    -> return $ Primary s failed next
    go failed (Always a) = return $ Fallback a failed

testSometimes :: Sometimes a -> IO (TestedSometimes a p)
testSometimes = go (TestedSometimes Nothing [] [])
  where
    go ts [] = return ts
    go ts (x:xs) = do
      r <- testSubfeature x
      case r of
        (Untestable fd' err) -> go (addFail ts (Left (fd' ,err))) xs
        (FailedFtr fd' errs) -> go (addFail ts (Right (fd' ,errs))) xs
        (SuccessfulFtr s)    -> return $ ts { tsSuccess = Just s }
    addFail ts@(TestedSometimes { tsFailed = f }) new
      = ts { tsFailed = new:f }

testSubfeature :: Subfeature a Tree -> IO (FeatureResult a p)
testSubfeature fd@(Subfeature { sfTree = t }) = do
  atm <- testActionTree t
  return $ either untestable checkAction atm
  where
    untestable (t', err) = Untestable (fd { sfTree = t' }) err
    checkAction (t', Just a, ms) = SuccessfulFtr
      $ Finished { finData = fd { sfTree = t' }
                 , finAction = a
                 , finWarnings = ms
                 }
    checkAction (t', Nothing, ms) = FailedFtr (fd { sfTree = t' }) ms

testActionTree :: ActionTree a Tree -> IO (ActionTreeMaybe a p)
testActionTree t = do
  case t of
    (IOTree a d)             -> do
      (t', a', msgs) <- doTest testIOTree d a
      return $ Right (IOTree a t', a', msgs)
    (DBusTree a (Just cl) d) -> do
      (t', a', msgs) <- doTest (testDBusTree cl) d a
      return $ Right (DBusTree a (Just cl) t', fmap (\f -> f cl) a', msgs)
    _                        -> return $ Left (t, "client not available")
  where
    doTest testFun d a = do
      (t', r) <- testFun d
      -- TODO actually recover the proper error messages
      let (a', msgs) = maybe (Nothing, []) (\p -> (fmap (apply a) p, [])) r
      return (t', a', msgs)
    apply (Standalone a) _ = a
    apply (Consumer a) p   = a p

testIOTree :: Tree (IODependency a p) p
  -> IO (ResultTree (IODependency a p) p, Maybe (Maybe p))
testIOTree = testTree testIODependency

testDBusTree :: Client -> Tree (DBusDependency a p) p
  -> IO (ResultTree (DBusDependency a p) p, Maybe (Maybe p))
testDBusTree client = testTree (testDBusDependency client)

testTree :: Monad m => (d -> m (Summary p)) -> Tree d p
  -> m (ResultTree d p, Maybe (Maybe p))
testTree test = go
  where
    go (And f a b) = do
      (ra, pa) <- go a
      let combine = maybe (const Nothing) (\pa' -> Just . f pa')
      let pass p = test2nd (combine p) ra b
      let fail_ = return (First ra b, Nothing)
      maybe fail_ pass pa
    go (Or fa fb a b) = do
      (ra, pa) <- go a
      let pass p = return (First ra b, Just $ fa <$> p)
      let fail_ = test2nd (Just . fb) ra b
      maybe fail_ pass pa
    go (Only a) =
      either (\es -> (LeafFail a es, Nothing)) (\(p, ws) -> (LeafSuccess a ws, Just p))
      <$> test a
    test2nd f ra b = do
      (rb, pb) <- go b
      return (Both ra rb, fmap (f =<<) pb)

testIODependency :: IODependency a p -> IO (Summary p)
testIODependency (Executable _ bin) = maybe err smryNil <$> findExecutable bin
  where
    err = Left ["executable '" ++ bin ++ "' not found"]

testIODependency (IOTest _ t) = maybe (Right (Nothing, [])) (Left . (:[])) <$> t

testIODependency (IORead _ t) = bimap (:[]) (, []) <$> t

testIODependency (Systemd t n) = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> Right (Nothing, [])
    _           -> Left ["systemd " ++ unitType t ++ " unit '" ++ n ++ "' not found"]
  where
    cmd = fmtCmd "systemctl" $ ["--user" | t == UserUnit] ++ ["status", n]
    unitType SystemUnit = "system"
    unitType UserUnit   = "user"

testIODependency (AccessiblePath p r w) = do
  res <- getPermissionsSafe p
  let msg = permMsg res
  return msg
  where
    testPerm False _ _  = Nothing
    testPerm True f res = Just $ f res
    permMsg NotFoundError            = smryFail "file not found"
    permMsg PermError                = smryFail "could not get permissions"
    permMsg (PermResult res) =
      case (testPerm r readable res, testPerm w writable res) of
        (Just False, Just False) -> smryFail "file not readable or writable"
        (Just False, _)          -> smryFail "file not readable"
        (_, Just False)          -> smryFail "file not writable"
        _                        -> Right (Nothing, [])

-- TODO actually collect errors here
testIODependency (NestedAlways a f) = do
  r <- testAlways a
  return $ Right $ case r of
    (Primary (Finished { finAction = act }) _ _) -> (Just $ f act, [])
    (Fallback act _)                             -> (Just $ f act, [])

testIODependency (NestedSometimes x f) = do
  TestedSometimes { tsSuccess = s, tsFailed = _ } <- testSometimes x
  return $ maybe (Left []) (\Finished { finAction = a } -> Right (Just $ f a, [])) s

testDBusDependency :: Client -> DBusDependency a p -> IO (Summary p)
testDBusDependency client (Bus bus) = do
  ret <- callMethod client queryBus queryPath queryIface queryMem
  return $ case ret of
        Left e    -> smryFail e
        Right b -> let ns = bodyGetNames b in
          if bus' `elem` ns then Right (Nothing, [])
          else smryFail $ unwords ["name", singleQuote bus', "not found on dbus"]
  where
    bus' = formatBusName bus
    queryBus = busName_ "org.freedesktop.DBus"
    queryIface = interfaceName_ "org.freedesktop.DBus"
    queryPath = objectPath_ "/"
    queryMem = memberName_ "ListNames"
    bodyGetNames [v] = fromMaybe [] $ fromVariant v :: [String]
    bodyGetNames _   = []

testDBusDependency client (Endpoint busname objpath iface mem) = do
  ret <- callMethod client busname objpath introspectInterface introspectMethod
  return $ case ret of
        Left e     -> smryFail e
        Right body -> procBody body
  where
    procBody body = let res = findMem =<< I.parseXML objpath =<< fromVariant
                          =<< listToMaybe body in
      case res of
        Just True -> Right (Nothing, [])
        _         -> smryFail $ fmtMsg' mem
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

testDBusDependency _ (DBusIO d) = testIODependency d

--------------------------------------------------------------------------------
-- | Printing

printMsgs :: LogLevel -> [Msg] -> IO ()
printMsgs lvl ms = do
  pn <- getProgName
  mapM_ (printMsg pn lvl) ms

printMsg :: String -> LogLevel -> Msg -> IO ()
printMsg pname lvl (Msg ml mn msg)
  | lvl > ml = putStrLn $ unwords [bracket pname, bracket mn, msg]
  | otherwise = skip
  where
    bracket s = "[" ++ s ++ "]"
