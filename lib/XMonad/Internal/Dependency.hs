--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeExe
  , UnitType(..)
  , Dependency(..)
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

data Dependency = Executable String
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

data Feature a = Feature
  { ftrAction   :: a
  , ftrSilent   :: Bool
  , ftrChildren :: [Dependency]
  }
  | ConstFeature a
  | BlankFeature

type FeatureX = Feature (X ())

type FeatureIO = Feature (IO ())

ioFeature :: (MonadIO m) => Feature (IO a) -> Feature (m a)
ioFeature f@Feature { ftrAction = a } = f { ftrAction = liftIO a }
ioFeature (ConstFeature f)            = ConstFeature $ liftIO f
ioFeature BlankFeature                = BlankFeature

evalFeature :: Feature a -> IO (MaybeExe a)
evalFeature (ConstFeature x) = return $ Right x
evalFeature BlankFeature = return $ Left []
evalFeature Feature { ftrAction = a, ftrSilent = s, ftrChildren = c } = do
  es <- mapM go c
  return $ case concat es of
    []  -> Right a
    es' -> Left (if s then [] else es')
  where
    go = fmap maybeToList . depInstalled

exe :: String -> Dependency
exe = Executable

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

-- TODO this is poorly named. This actually represents an action that has
-- one or more dependencies (where "action" is not necessarily executing an exe)
type MaybeExe a = Either [String] a

type MaybeX = MaybeExe (X ())

featureRun :: [Dependency] -> a -> Feature a
featureRun ds x = Feature
  { ftrAction = x
  , ftrSilent = False
  , ftrChildren = ds
  }

featureSpawnCmd :: MonadIO m => String -> [String] -> Feature (m ())
featureSpawnCmd cmd args = featureRun [exe cmd] $ spawnCmd cmd args

featureSpawn :: MonadIO m => String -> Feature (m ())
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

depInstalled :: Dependency -> IO (Maybe String)
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
