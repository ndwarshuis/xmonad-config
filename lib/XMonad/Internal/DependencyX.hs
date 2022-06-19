{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.DependencyX where

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
-- | Feature

data AnyFeature p = FX (FeatureX p) | FIO (FeatureIO p)

type FeatureX p = Feature (X ()) Tree p

type FeatureIO p = Feature (IO ()) Tree p

data Feature a t p = Feature (FeatureData a t p) (Feature a t p)
  | NoFeature
  | ConstFeature a

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

-- | Given a feature, return a monadic action if all dependencies are satisfied,
-- else Nothing (and print errors)
evalFeature :: Feature a ResultTree p -> ([Msg], Maybe a)
evalFeature (ConstFeature x) = ([], Just x)
evalFeature NoFeature = return Nothing
evalFeature (Feature f alt) =
  either (\es -> first (++es) $ evalFeature alt) (\a -> ([], Just a))
  $ evalFeatureData f

--------------------------------------------------------------------------------
-- | Feature Data

data FeatureData a t p = FeatureData
  { fdTree  :: ActionTree a t p
  , fdName  :: String
  , fdLevel :: LogLevel
  }

data LogLevel = Silent | Error | Warn | Debug deriving (Eq, Show, Ord)

data Msg = Msg LogLevel String String

evalFeatureData :: FeatureData a ResultTree p -> Either [Msg] a
evalFeatureData FeatureData { fdTree = t, fdName = n, fdLevel = l } =
  either (Left . fmap (Msg l n)) Right $ evalActionTree t

--------------------------------------------------------------------------------
-- | Action Tree

data ActionTree a t p =
  IOTree (Action a p) (t (IODependency a t p) p)
  | DBusTree (Action (Client -> a) p) (Maybe Client) (t (DBusDependency a t p) p)

data Action a p = Standalone a | Consumer (p -> a) (p -> p -> Result p)

evalActionTree :: ActionTree a ResultTree p -> Either [String] a
evalActionTree at = case at of
  (IOTree a t)            -> resolve a t
  (DBusTree a (Just c) t) -> (\f -> f c) <$> resolve a t
  -- TODO this is kinda redundant because I'll also get a message the dep tree
  -- failing what I don't have a client
  (DBusTree _ Nothing _)  -> Left ["client not available to build action"]
  where
    resolve (Standalone f) t       = const (Right f) =<< evalTreeNoop t
    resolve (Consumer f combine) t = maybe noPayload (Right . f)
      -- TODO not sure about this Right . Just thing, seems odd that I need it
      =<< evalTree combine (Right . Just) t
    noPayload = Left ["payload not available to build action"]

--------------------------------------------------------------------------------
-- | (Result) Tree

data Tree d p = And (Tree d p) (Tree d p) | Or (Tree d p) (Tree d p) | Only d

-- | how to interpret ResultTree combinations:
-- First (Success a) (Tree a) -> Or that succeeded on left
-- First (Fail a) (Tree a) -> And that failed on left
-- Both (Fail a) (Fail a) -> Or that failed
-- Both (Success a) (Success a) -> And that succeeded
-- Both (Fail a) (Success a) -> Or that failed first and succeeded second
-- Both (Success a) (Fail a) -> And that failed on the right

data ResultTree d p =
  First (ResultTree d p) (Tree d p)
  | Both (ResultTree d p) (ResultTree d p)
  | Success d (Maybe p)
  | Fail d [String]

-- | Given an updated condition tree, collect all evaluations and return a
-- combined evaluation (which may be Nothing, Something, or an error). Must also
-- supply a function to combine Results in the corner case where two And
-- arguments are successful and have non-empty outputs.
evalTree :: (p -> p -> Result p) -> (p -> Result p) -> ResultTree a p -> Result p
evalTree f2 f1 = go (Right Nothing)
  where
    go acc (First a _) = case go acc a of
      -- Or succeeds on left
      (Right p) -> combine p =<< acc
      -- And fails on left
      (Left e)  -> Left e
    go acc (Both a b) = case (go acc a, go acc b) of
      -- And succeeds
      (Right pa, Right pb) -> combine pb =<< combine pa =<< acc
      -- Or fails both
      (Left ea, Left eb)   -> addErrors acc (ea ++ eb)
      -- And fails on right
      (Right _, Left eb)   -> addErrors acc eb
      -- Or succeeds on right
      (Left ea, Right pb)  -> either (Left . (ea ++)) (combine pb) acc
    go acc (Success _ p) = combine p =<< acc
    go acc (Fail _ e)    = addErrors acc e
    addErrors cur new = Left $ new ++ fromLeft [] cur
    combine (Just a) (Just b) = f2 a b
    combine (Just a) Nothing  = f1 a
    combine Nothing (Just b)  = f1 b
    combine _ _               = Right Nothing

evalTreeNoop :: ResultTree a p -> Result p
evalTreeNoop = evalTree (const . evalNil) evalNil

mapMTree :: Monad m => (d -> m (Result p)) -> Tree d p
  -> m (ResultTree d p)
mapMTree f = fmap snd . go
  where
    go (And a b) = doTest a b True
    go (Or a b) = doTest a b False
    go (Only a) = either (\x -> (False, Fail a x)) (\x -> (True, Success a x))
      <$> f a
    doTest a b useAnd = do
      (success, ra) <- go a
      let try2nd = if useAnd then success else not success
      if try2nd then second (Both ra) <$> go b else return (success, First ra b)

--------------------------------------------------------------------------------
-- | Dependency

data IODependency a t p = Executable Bool FilePath
  | AccessiblePath FilePath Bool Bool
  | IOTest String (IO (Maybe String))
  | IORead String (IO (Either String (Maybe p)))
  | Systemd UnitType String
  | NestedFeature (Feature a t p)

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

data DBusDependency a e p =
  Bus BusName
  | Endpoint BusName ObjectPath InterfaceName DBusMember
  | DBusIO (IODependency a e p)

data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Result

type Result p = Either [String] (Maybe p)

evalNil :: p -> Result q
evalNil = const $ Right Nothing

evalFail :: String -> Result p
evalFail msg = Left [msg]

-- | Given a condition tree, evaluate all dependencies according to 'fill in'
-- the results (which may either be Nothing, a returned payload to use for the
-- action, or an error.
updateIOConditions :: Tree (IODependency a Tree p) p -> IO (ResultTree (IODependency a Tree p) p)
updateIOConditions = mapMTree testIODependency

updateDBusConditions :: Client -> Tree (DBusDependency a Tree p) p
  -> IO (ResultTree (DBusDependency a Tree p) p)
updateDBusConditions client = mapMTree (evalDBusDependency client)

--------------------------------------------------------------------------------
-- | IO Dependency

testIODependency :: IODependency a Tree p -> IO (Result p)

testIODependency (Executable _ bin) = maybe err evalNil <$> findExecutable bin
  where
    err = Left ["executable '" ++ bin ++ "' not found"]

testIODependency (IOTest _ t) = maybe (Right Nothing) (Left . (:[])) <$> t

testIODependency (IORead _ t) = either (Left . (:[])) Right <$> t

testIODependency (Systemd t n) = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> Right Nothing
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
    permMsg NotFoundError            = evalFail "file not found"
    permMsg PermError                = evalFail "could not get permissions"
    permMsg (PermResult res) =
      case (testPerm r readable res, testPerm w writable res) of
        (Just False, Just False) -> evalFail "file not readable or writable"
        (Just False, _)          -> evalFail "file not readable"
        (_, Just False)          -> evalFail "file not writable"
        _                        -> Right Nothing

testIODependency (NestedFeature ftr) = go ftr
  where
    go (Feature (FeatureData { fdTree = t }) alt) =
      -- TODO add feature name to messages
      case t of
        (IOTree a ct)                 -> evalFun a <$> updateIOConditions ct
        (DBusTree a (Just client) ct) -> evalFun a <$> updateDBusConditions client ct
        (DBusTree _ Nothing _)        -> failMaybe alt ["client not found"]
      where
        failMaybe NoFeature msg = return $ Left msg
        failMaybe f _           = go f
        evalFun (Standalone _) = evalTreeNoop
        evalFun (Consumer _ f) = evalTree f (return . return)
    go _ = return $ Right Nothing

--------------------------------------------------------------------------------
-- | DBus Dependency Result

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

evalDBusDependency :: Client -> DBusDependency a Tree p -> IO (Result p)

evalDBusDependency client (Bus bus) = do
  ret <- callMethod client queryBus queryPath queryIface queryMem
  return $ case ret of
        Left e    -> evalFail e
        Right b -> let ns = bodyGetNames b in
          if bus' `elem` ns then Right Nothing
          else evalFail $ unwords ["name", singleQuote bus', "not found on dbus"]
  where
    bus' = formatBusName bus
    queryBus = busName_ "org.freedesktop.DBus"
    queryIface = interfaceName_ "org.freedesktop.DBus"
    queryPath = objectPath_ "/"
    queryMem = memberName_ "ListNames"
    bodyGetNames [v] = fromMaybe [] $ fromVariant v :: [String]
    bodyGetNames _   = []

evalDBusDependency client (Endpoint busname objpath iface mem) = do
  ret <- callMethod client busname objpath introspectInterface introspectMethod
  return $ case ret of
        Left e     -> evalFail e
        Right body -> procBody body
  where
    procBody body = let res = findMem =<< I.parseXML objpath =<< fromVariant
                          =<< listToMaybe body in
      case res of
        Just True -> Right Nothing
        _         -> evalFail $ fmtMsg' mem
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

evalDBusDependency _ (DBusIO d) = testIODependency d
