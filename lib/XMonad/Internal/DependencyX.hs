{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.DependencyX where

-- import           Control.Monad.IO.Class
-- import           Control.Monad.Identity

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

-- import           XMonad.Core             (X, io)
import           XMonad.Core             (X)
import           XMonad.Internal.IO
import           XMonad.Internal.Process
import           XMonad.Internal.Shell

--------------------------------------------------------------------------------
-- | Feature

data AnyFeature p = FX (FeatureX p) | FIO (FeatureIO p)

type FeatureX p = Feature (X ()) p

type FeatureIO p = Feature (IO ()) p

data Feature a p = Feature (FeatureData a Tree p) (Feature a p)
  | NoFeature
  | ConstFeature a

-- TODO this feels icky, and I don't feel like typing it
data TestedFeature a p = TestedFeature (TestedFeature_ a p)
  | TestedConst a [FailedFeature a p]

data TestedFeature_ a p = TestedFeature_
  { tfSuccess  :: Maybe (SuccessfulFeature a p)
  , tfFailed   :: [FailedFeature a p]
  , tfUntested :: Feature a p
  }

type FailedFeature a p = Either (FeatureData a Tree p, String)
  (FeatureData a ResultTree p, [String])

data SuccessfulFeature a p = SuccessfulFeature
  { sfData     :: FeatureData a ResultTree p
  , sfAction   :: a
  , sfWarnings :: [String]
  }

data FeatureResult a p = Untestable (FeatureData a Tree p) String |
  FailedFtr (FeatureData a ResultTree p) [String] |
  SuccessfulFtr (SuccessfulFeature a p)

type ActionTreeMaybe a p = Either (ActionTree a Tree p, String)
  (ActionTree a ResultTree p, Maybe a, [String])

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

--------------------------------------------------------------------------------
-- | Feature Data

data FeatureData a t p = FeatureData
  { fdTree  :: ActionTree a t p
  , fdName  :: String
  , fdLevel :: LogLevel
  }

data LogLevel = Silent | Error | Warn | Debug deriving (Eq, Show, Ord)

data Msg = Msg LogLevel String String

--------------------------------------------------------------------------------
-- | Action Tree

data ActionTree a t p =
  IOTree (Action a p) (t (IODependency a p) p)
  | DBusTree (Action (Client -> a) p) (Maybe Client) (t (DBusDependency a p) p)

data Action a p = Standalone a | Consumer (p -> a)

--------------------------------------------------------------------------------
-- | (Result) Tree

data Tree d p =
  And (p -> p -> p) (Tree d p) (Tree d p)
  | Or (p -> p) (p -> p) (Tree d p) (Tree d p)
  | Only d

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
  | NestedFeature (Feature a p) (a -> p)

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

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

--------------------------------------------------------------------------------
-- | Feature evaluation
--
-- Here we attempt to build and return the monadic actions encoded by each
-- feature.

evalFeature :: Feature a p -> IO (Maybe a)
evalFeature ftr = do
  r <- testFeature ftr
  -- TODO print out all the errors/warnings when doing this
  case r of
    TestedConst c _ -> return $ Just c
    TestedFeature t ->
      case t of
        TestedFeature_ { tfSuccess = Nothing, tfFailed = _ } -> return Nothing
        TestedFeature_ { tfSuccess = Just (SuccessfulFeature { sfAction = a })
                       , tfFailed = _ }  -> return $ Just a

--------------------------------------------------------------------------------
-- | Dependency Testing
--
-- Here we test all dependencies and keep the tree structure so we can print it
-- for diagnostic purposes. This obviously has overlap with feature evaluation
-- since we need to resolve dependencies to build each feature.

testFeature :: Feature a p -> IO (TestedFeature a p)
testFeature = go []
  where
    go failed (Feature fd alt) = do
      r <- testFeatureData fd
      case r of
        (Untestable fd' err) -> tryAlt alt $ Left (fd' ,err):failed
        (FailedFtr fd' errs) -> tryAlt alt $ Right (fd' ,errs):failed
        (SuccessfulFtr s)    -> return $ TestedFeature $ TestedFeature_ (Just s) failed alt
    go failed NoFeature = return $ TestedFeature $ TestedFeature_ Nothing failed NoFeature
    go failed (ConstFeature c) = return $ TestedConst c failed
    tryAlt NoFeature failed = return $ TestedFeature $ TestedFeature_ Nothing failed NoFeature
    tryAlt alt failed       = go failed alt

testFeatureData :: FeatureData a Tree p -> IO (FeatureResult a p)
testFeatureData fd@(FeatureData { fdTree = t }) = do
  atm <- testActionTree t
  return $ either untestable checkAction atm
  where
    untestable (t', err) = Untestable (fd { fdTree = t' }) err
    checkAction (t', Just a, ms) = SuccessfulFtr
      $ SuccessfulFeature { sfData = fd { fdTree = t' }
                          , sfAction = a
                          , sfWarnings = ms
                          }
    checkAction (t', Nothing, ms) = FailedFtr (fd { fdTree = t' }) ms

testActionTree :: ActionTree a Tree p -> IO (ActionTreeMaybe a p)
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

testIODependency (NestedFeature ftr trans) = do
  r <- testFeature ftr
  return $ case r of
    -- TODO why would anyone do this?
    TestedConst c _ -> Right (Just $ trans c, [])
    TestedFeature t ->
      case t of
        -- TODO actually summarize errors
        TestedFeature_ { tfSuccess = Nothing
                       , tfFailed = _ } -> Left []
        TestedFeature_ { tfSuccess = Just (SuccessfulFeature { sfAction = a })
                       , tfFailed = _ }  -> Right (Just $ trans a, [])
-- testIODependency (NestedFeature ftr) = go ftr
--   where
--     go (Feature (FeatureData { fdTree = t }) alt) =
--       -- TODO add feature name to messages
--       case t of
--         (IOTree _ ct)             -> summarize <$> testIOTree ct
--         (DBusTree _ (Just cl) ct) -> summarize <$> testDBusTree cl ct
--         (DBusTree _ Nothing _)    -> failMaybe alt ["client not found"]
--       where
--         failMaybe NoFeature msg = return $ Left msg
--         failMaybe f _           = go f
--         -- TODO actually thread errors here
--         summarize (_, Just p)  = Right (p, [])
--         summarize (_, Nothing) = Left []
--     go _ = return $ Right (Nothing, [])

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
