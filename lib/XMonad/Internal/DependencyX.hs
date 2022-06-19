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
evalFeature :: Feature a ResultTree p -> (Maybe a, [Msg])
evalFeature (ConstFeature x) = (Just x, [])
evalFeature NoFeature = (Nothing, [])
evalFeature (Feature f alt) =
  either (\es -> second (++es) $ evalFeature alt) (first Just)
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

evalFeatureData :: FeatureData a ResultTree p -> Either [Msg] (a, [Msg])
evalFeatureData FeatureData { fdTree = t, fdName = n, fdLevel = l } =
  bimap (msg l) (second (msg $ min l Warn)) $ evalActionTree t
  where
    msg lvl = fmap (Msg lvl n)

--------------------------------------------------------------------------------
-- | Action Tree

data ActionTree a t p =
  IOTree (Action a p) (t (IODependency a t p) p)
  | DBusTree (Action (Client -> a) p) (Maybe Client) (t (DBusDependency a t p) p)

data Action a p = Standalone a
  | Consumer (p -> a) (p -> Summary p) (p -> p -> Summary p)

evalActionTree :: ActionTree a ResultTree p -> Either [String] (a, [String])
evalActionTree at = case at of
  (IOTree a t)            -> resolve a t
  (DBusTree a (Just c) t) -> (\(f, w) -> (f c, w)) <$> resolve a t
  -- TODO this is kinda redundant because I'll also get a message the dep tree
  -- failing when I don't have a client
  (DBusTree _ Nothing _)  -> Left ["client not available to build action"]
  where
    resolve (Standalone af) t = (\(_, w) -> Right (af, w)) =<< evalTreeNoop t
    resolve (Consumer af f1 f2) t = (\(p, w) -> maybe noPayload (\p' -> Right (af p', w)) p)
      =<< evalTree f1 f2 t
    noPayload = Left ["payload not available to build action"]

--------------------------------------------------------------------------------
-- | (Result) Tree

data Tree d p = And (Tree d p) (Tree d p) | Or (Tree d p) (Tree d p) | Only d

-- | how to interpret ResultTree combinations:
-- First (LeafSuccess a) (Tree a) -> Or that succeeded on left
-- First (LeafFail a) (Tree a) -> And that failed on left
-- Both (LeafFail a) (Fail a) -> Or that failed
-- Both (LeafSuccess a) (LeafSuccess a) -> And that succeeded
-- Both (LeafFail a) (LeafSuccess a) -> Or that failed first and succeeded second
-- Both (LeafSuccess a) (LeafFail a) -> And that failed on the right

data ResultTree d p =
  First (ResultTree d p) (Tree d p)
  | Both (ResultTree d p) (ResultTree d p)
  | LeafSuccess d (Maybe p, [String])
  | LeafFail d [String]

type Payload p = (Maybe p, [String])

type Summary p = Either [String] (Payload p)

smryNil :: q -> Summary p
smryNil = const $ Right (Nothing, [])

smryFail :: String -> Either [String] a
smryFail msg = Left [msg]

smryInit :: Summary p
smryInit = Right (Nothing, [])

-- | Given an updated condition tree, collect all evaluations and return a
-- combined evaluation (which may be Nothing, Something, or an error). Must also
-- supply a function to combine Results in the corner case where two And
-- arguments are successful and have non-empty outputs.
evalTree :: (p -> Summary p) -> (p -> p -> Summary p) -> ResultTree a p -> Summary p
evalTree f1 f2 = go (Right (Nothing, []))
  where
    go smry (First a _) = case go smry a of
    --   -- Or succeeds on left
      (Right p) -> combine p =<< smry
    --   -- And fails on left
      (Left e)  -> Left e
    go smry (Both a b) = case (go smry a, go smry b) of
      -- And succeeds
      (Right pa, Right pb) -> combine pb =<< combine pa =<< smry
      -- Or fails both
      (Left ea, Left eb)   -> addCrits smry (ea ++ eb)
      -- And fails on right
      (Right _, Left eb)   -> addCrits smry eb
      -- -- Or succeeds on right
      (Left ea, Right pb)  -> addWarnings ea =<< combine pb =<< smry
    go smry (LeafSuccess _ s) = combine s =<< smry
    go smry (LeafFail _ e)    = addCrits smry e
    combine (Just pa, wa) (Just pb, _) = addWarnings wa =<< f2 pa pb
    combine (Just pa, wa) (Nothing, _) = addWarnings wa =<< f1 pa
    combine (Nothing, wa) (Just pb, _) = addWarnings wa =<< f1 pb
    combine (Nothing, wa) cur          = addWarnings wa cur
    addWarnings new (p, cur) = Right (p, cur ++ new)
    addCrits smry crits = Left $ crits ++ fromLeft [] smry

evalTreeNoop :: ResultTree a p -> Summary p
evalTreeNoop = evalTree smryNil (const . smryNil)

--------------------------------------------------------------------------------
-- | Result

type Result p = Either [String] (Maybe p)

resultNil :: p -> Result q
resultNil = const $ Right Nothing

-- | Given a condition tree, evaluate all dependencies according to 'fill in'
-- the results (which may either be Nothing, a returned payload to use for the
-- action, or an error.
updateIOConditions :: Tree (IODependency a Tree p) p
  -> IO (ResultTree (IODependency a Tree p) p)
updateIOConditions = mapMTree testIODependency

updateDBusConditions :: Client -> Tree (DBusDependency a Tree p) p
  -> IO (ResultTree (DBusDependency a Tree p) p)
updateDBusConditions client = mapMTree (evalDBusDependency client)

mapMTree :: Monad m => (d -> m (Summary p)) -> Tree d p -> m (ResultTree d p)
mapMTree f = fmap snd . go
  where
    go (And a b) = doTest a b True
    go (Or a b) = doTest a b False
    go (Only a) =
      either (\es -> (False, LeafFail a es)) (\p -> (True, LeafSuccess a p))
      <$> f a
    doTest a b useAnd = do
      (success, ra) <- go a
      let try2nd = if useAnd then success else not success
      if try2nd then second (Both ra) <$> go b else return (success, First ra b)

--------------------------------------------------------------------------------
-- | IO Dependency

data IODependency a t p = Executable Bool FilePath
  | AccessiblePath FilePath Bool Bool
  | IOTest String (IO (Maybe String))
  | IORead String (IO (Either String (Maybe p)))
  | Systemd UnitType String
  | NestedFeature (Feature a t p)

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

testIODependency :: IODependency a Tree p -> IO (Summary p)

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
        evalFun (Standalone _)     = evalTreeNoop
        evalFun (Consumer _ f1 f2) = evalTree f1 f2
    go _ = return $ Right (Nothing, [])

--------------------------------------------------------------------------------
-- | DBus Dependency Result

data DBusDependency a e p =
  Bus BusName
  | Endpoint BusName ObjectPath InterfaceName DBusMember
  | DBusIO (IODependency a e p)

data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

evalDBusDependency :: Client -> DBusDependency a Tree p -> IO (Summary p)

evalDBusDependency client (Bus bus) = do
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

evalDBusDependency client (Endpoint busname objpath iface mem) = do
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

evalDBusDependency _ (DBusIO d) = testIODependency d
