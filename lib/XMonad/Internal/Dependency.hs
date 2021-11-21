--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeAction
  , MaybeX
  , FeatureX
  , FeatureIO
  , Feature(..)
  , Warning(..)
  , Dependency(..)
  , UnitType(..)
  , Bus(..)
  , Endpoint(..)
  , DBusMember(..)
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
  , warnMissing
  , whenSatisfied
  , ifSatisfied
  , executeFeature
  , executeFeature_
  , applyFeature
  , applyFeature_
  ) where

import           Control.Monad.IO.Class

import           Data.Bifunctor          (bimap)
import           Data.List               (find)
import           Data.Maybe              (catMaybes, fromMaybe, listToMaybe)

import           DBus
import           DBus.Client
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
-- A 'feature' is an 'action' (usually an IO ()) that requires one or more
-- 'dependencies'. Features also have a useful name and an error logging
-- protocol.
--
-- NOTE: there is no way to make a feature depend on another feature. This is
-- very complicated to implement and would only be applicable to a few instances
-- (notable the dbus interfaces). In order to implement a dependency tree, use
-- dependencies that target the output/state of another feature; this is more
-- robust anyways, at the cost of being a bit slower.

data Feature a = Feature
  { ftrMaybeAction :: a
  , ftrName        :: String
  , ftrWarning     :: Warning
  , ftrChildren    :: [Dependency]
  }
  | ConstFeature a
  | BlankFeature

-- TODO this is silly as is, and could be made more useful by representing
-- loglevels
data Warning = Silent | Default

type FeatureX = Feature (X ())

type FeatureIO = Feature (IO ())

ioFeature :: (MonadIO m) => Feature (IO a) -> Feature (m a)
ioFeature f@Feature { ftrMaybeAction = a } = f { ftrMaybeAction = liftIO a }
ioFeature (ConstFeature f)                 = ConstFeature $ liftIO f
ioFeature BlankFeature                     = BlankFeature

featureDefault :: String -> [Dependency] -> a -> Feature a
featureDefault n ds x = Feature
  { ftrMaybeAction = x
  , ftrName = n
  , ftrWarning = Default
  , ftrChildren = ds
  }

featureExe :: MonadIO m => String -> String -> Feature (m ())
featureExe n cmd = featureExeArgs n cmd []

featureExeArgs :: MonadIO m => String -> String -> [String] -> Feature (m ())
featureExeArgs n cmd args =
  featureDefault n [Executable cmd] $ spawnCmd cmd args

--------------------------------------------------------------------------------
-- | Feature evaluation
--
-- Evaluate a feature by testing if its dependencies are satisfied, and return
-- either the action of the feature or 0 or more error messages that signify
-- what dependencies are missing and why.

type MaybeAction a = Either [String] a

type MaybeX = MaybeAction (X ())

evalFeature :: Feature a -> IO (MaybeAction a)
evalFeature (ConstFeature x) = return $ Right x
evalFeature BlankFeature = return $ Left []
evalFeature Feature
  { ftrMaybeAction = a
  , ftrName = n
  , ftrWarning = w
  , ftrChildren = c
  } = do
  procName <- getProgName
  es <- catMaybes <$> mapM evalDependency c
  return $ case es of
    []  -> Right a
    es' -> Left $ fmtWarnings procName es'
  where
    fmtWarnings procName es = case w of
      Silent  -> []
      Default -> fmap (fmtMsg procName "WARNING" . ((n ++ " disabled; ") ++)) es

applyFeature :: MonadIO m => (m a -> m a) -> a -> Feature (IO a) -> m a
applyFeature iof def ftr = do
  a <- io $ evalFeature ftr
  either (\es -> io $ warnMissing' es >> return def) (iof . io) a

applyFeature_ :: MonadIO m => (m () -> m ()) -> Feature (IO ()) -> m ()
applyFeature_ iof = applyFeature iof ()

executeFeature :: MonadIO m => a -> Feature (IO a) -> m a
executeFeature = applyFeature id

executeFeature_ :: Feature (IO ()) -> IO ()
executeFeature_ = executeFeature ()

whenSatisfied :: Monad m => MaybeAction (m ()) -> m ()
whenSatisfied = flip ifSatisfied skip

ifSatisfied ::  MaybeAction a -> a -> a
ifSatisfied (Right x) _ = x
ifSatisfied _ alt       = alt

--------------------------------------------------------------------------------
-- | Dependencies

data Dependency = Executable String
  | AccessiblePath FilePath Bool Bool
  | IOTest (IO (Maybe String))
  | DBusEndpoint Bus Endpoint
  | DBusBus Bus
  | Systemd UnitType String

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

data Bus = Bus Bool BusName deriving (Eq, Show)

data Endpoint = Endpoint ObjectPath InterfaceName DBusMember deriving (Eq, Show)

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
-- | Dependency evaluation
--
-- Test the existence of dependencies and return either Nothing (which actually
-- means success) or Just <error message>.

evalDependency :: Dependency -> IO (Maybe String)
evalDependency (Executable n)         = exeSatisfied n
evalDependency (IOTest t)             = t
evalDependency (Systemd t n)          = unitSatisfied t n
evalDependency (AccessiblePath p r w) = pathSatisfied p r w
evalDependency (DBusEndpoint b e)     = endpointSatisfied b e
evalDependency (DBusBus b)            = busSatisfied b

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

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

callMethod :: Bus -> ObjectPath -> InterfaceName -> MemberName -> IO (Either String [Variant])
callMethod (Bus usesys bus) path iface mem = do
  client <- if usesys then connectSystem else connectSession
  reply <- call client (methodCall path iface mem)
           { methodCallDestination = Just bus }
  disconnect client
  return $ bimap methodErrorMessage methodReturnBody reply

busSatisfied :: Bus -> IO (Maybe String)
busSatisfied (Bus usesystem bus) = do
  ret <- callMethod (Bus usesystem queryBus) queryPath queryIface queryMem
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

endpointSatisfied :: Bus -> Endpoint -> IO (Maybe String)
endpointSatisfied b@(Bus _ bus) (Endpoint objpath iface mem) = do
  ret <- callMethod b objpath introspectInterface introspectMethod
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
      , formatBusName bus
      ]

--------------------------------------------------------------------------------
-- | Logging functions

warnMissing :: [MaybeAction a] -> IO ()
warnMissing xs = warnMissing' $ concat $ [ m | (Left m) <- xs ]

warnMissing' :: [String] -> IO ()
warnMissing' = mapM_ putStrLn

fmtMsg :: String -> String -> String -> String
fmtMsg procName level msg = unwords [bracket procName, bracket level, msg]
  where
    bracket s = "[" ++ s ++ "]"
