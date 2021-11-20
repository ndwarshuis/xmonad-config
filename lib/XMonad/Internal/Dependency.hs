--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeExe
  , UnitType(..)
  , Dependency(..)
  , DependencyData(..)
  , DBusMember(..)
  , MaybeX
  , FeatureX
  , FeatureIO
  , Feature(..)
  , ioFeature
  , evalFeature
  , exe
  , systemUnit
  , userUnit
  , pathR
  , pathW
  , pathRW
  , featureRun
  , featureSpawnCmd
  , featureSpawn
  , warnMissing
  , whenInstalled
  , ifInstalled
  , fmtCmd
  , spawnCmd
  ) where

import           Control.Monad.IO.Class

import           Data.List               (find)
import           Data.Maybe              (listToMaybe, maybeToList)

import           DBus
import           DBus.Client
import qualified DBus.Introspection      as I

import           System.Directory        (findExecutable, readable, writable)
import           System.Exit

import           XMonad.Core             (X)
import           XMonad.Internal.IO
import           XMonad.Internal.Process
import           XMonad.Internal.Shell

--------------------------------------------------------------------------------
-- | Gracefully handling missing binaries

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

data DBusMember = Method_ MemberName
  | Signal_ MemberName
  | Property_ String
  deriving (Eq, Show)

data DependencyData = Executable String
  | AccessiblePath FilePath Bool Bool
  | IOTest (IO (Maybe String))
  | DBusEndpoint
    { ddDbusBus       :: BusName
    , ddDbusSystem    :: Bool
    , ddDbusObject    :: ObjectPath
    , ddDbusInterface :: InterfaceName
    , ddDbusMember    :: DBusMember
    }
  | Systemd UnitType String

data Dependency a = SubFeature (Feature a a)
  | Dependency DependencyData

data Feature a b = Feature
  { ftrAction   :: a
  , ftrSilent   :: Bool
  , ftrChildren :: [Dependency b]
  }
  | ConstFeature a
  | BlankFeature

type FeatureX = Feature (X ()) (X ())

type FeatureIO = Feature (IO ()) (IO ())

ioFeature :: (MonadIO m, MonadIO n) => Feature (IO a) (IO b) -> Feature (m a) (n b)
ioFeature f@Feature { ftrAction = a, ftrChildren = ds } =
  f { ftrAction = liftIO a, ftrChildren = fmap go ds }
  where
    go :: MonadIO o => Dependency (IO b) -> Dependency (o b)
    go (SubFeature s) = SubFeature $ ioFeature s
    go (Dependency d) = Dependency d
ioFeature (ConstFeature f) = ConstFeature $ liftIO f
ioFeature BlankFeature = BlankFeature

evalFeature :: Feature a b -> IO (MaybeExe a)
evalFeature (ConstFeature x) = return $ Right x
evalFeature BlankFeature = return $ Left []
evalFeature Feature { ftrAction = a, ftrSilent = s, ftrChildren = c } = do
  es <- mapM go c
  return $ case concat es of
    []  -> Right a
    es' -> Left (if s then [] else es')
  where
    go (SubFeature Feature { ftrChildren = cs }) = concat <$> mapM go cs
    go (Dependency d) = do
      e <- depInstalled d
      return $ maybeToList e
    go (SubFeature _) = return []

exe :: String -> Dependency a
exe = Dependency . Executable

unit :: UnitType -> String -> Dependency a
unit t = Dependency . Systemd t

path :: Bool -> Bool -> String -> Dependency a
path r w n = Dependency $ AccessiblePath n r w

pathR :: String -> Dependency a
pathR = path True False

pathW :: String -> Dependency a
pathW = path False True

pathRW :: String -> Dependency a
pathRW = path True True

systemUnit :: String -> Dependency a
systemUnit = unit SystemUnit

userUnit :: String -> Dependency a
userUnit = unit UserUnit

-- TODO this is poorly named. This actually represents an action that has
-- one or more dependencies (where "action" is not necessarily executing an exe)
type MaybeExe a = Either [String] a

type MaybeX = MaybeExe (X ())

featureRun :: [Dependency a] -> b -> Feature b a
featureRun ds x = Feature
  { ftrAction = x
  , ftrSilent = False
  , ftrChildren = ds
  }

featureSpawnCmd :: MonadIO m => String -> [String] -> Feature (m ()) (m ())
featureSpawnCmd cmd args = featureRun [exe cmd] $ spawnCmd cmd args

featureSpawn :: MonadIO m => String -> Feature (m ()) (m ())
featureSpawn cmd = featureSpawnCmd cmd []

exeInstalled :: String -> IO (Maybe String)
exeInstalled x = do
  r <- findExecutable x
  return $ case r of
    (Just _) -> Nothing
    _        -> Just $ "executable '" ++ x ++ "' not found"

unitInstalled :: UnitType -> String -> IO (Maybe String)
unitInstalled u x = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> Nothing
    _           -> Just $ "systemd " ++ unitType u ++ " unit '" ++ x ++ "' not found"
  where
    cmd = fmtCmd "systemctl" $ ["--user" | u == UserUnit] ++ ["status", x]
    unitType SystemUnit = "system"
    unitType UserUnit   = "user"

pathAccessible :: FilePath -> Bool -> Bool -> IO (Maybe String)
pathAccessible p testread testwrite = do
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

dbusInstalled :: BusName -> Bool -> ObjectPath -> InterfaceName -> DBusMember
  -> IO (Maybe String)
dbusInstalled bus usesystem objpath iface mem = do
  client <- if usesystem then connectSystem else connectSession
  reply <- call_ client (methodCall objpath introspectInterface introspectMethod)
           { methodCallDestination = Just bus }
  let res = findMem =<< I.parseXML objpath =<< fromVariant
        =<< listToMaybe (methodReturnBody reply)
  disconnect client
  return $ case res of
    Just _ -> Nothing
    _      -> Just "some random dbus interface not found"
  where
    findMem obj = fmap (matchMem mem)
      $ find (\i -> I.interfaceName i == iface)
      $ I.objectInterfaces obj
    matchMem (Method_ n) = elem n . fmap I.methodName . I.interfaceMethods
    matchMem (Signal_ n) = elem n . fmap I.signalName . I.interfaceSignals
    matchMem (Property_ n) = elem n . fmap I.propertyName . I.interfaceProperties

depInstalled :: DependencyData -> IO (Maybe String)
depInstalled (Executable n)         = exeInstalled n
depInstalled (IOTest t)             = t
depInstalled (Systemd t n)          = unitInstalled t n
depInstalled (AccessiblePath p r w) = pathAccessible p r w
depInstalled DBusEndpoint { ddDbusBus = b
                          , ddDbusSystem = s
                          , ddDbusObject = o
                          , ddDbusInterface = i
                          , ddDbusMember = m
                          } = dbusInstalled b s o i m

whenInstalled :: Monad m => MaybeExe (m ()) -> m ()
whenInstalled = flip ifInstalled skip

ifInstalled ::  MaybeExe a -> a -> a
ifInstalled (Right x) _ = x
ifInstalled _ alt       = alt

warnMissing :: [MaybeExe a] -> IO ()
warnMissing xs = mapM_ putStrLn $ fmap ("[WARNING] "++) $ concat $ [ m | (Left m) <- xs ]
