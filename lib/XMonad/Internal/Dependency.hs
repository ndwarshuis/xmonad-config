{-# LANGUAGE DeriveTraversable #-}

--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeExe(..)
  , UnitType(..)
  , Dependency(..)
  , DependencyData(..)
  , DBusMember(..)
  , MaybeX
  , Feature(..)
  , evalFeature
  , exe
  , systemUnit
  , userUnit
  , pathR
  , pathW
  , pathRW
  , checkInstalled
  , runIfInstalled
  , depInstalled
  , warnMissing
  , whenInstalled
  , ifInstalled
  , spawnIfInstalled
  , spawnCmdIfInstalled
  , noCheck
  , fmtCmd
  , spawnCmd
  , doubleQuote
  , singleQuote
  , (#!&&)
  , (#!||)
  , (#!|)
  , (#!>>)
  , playSound
  , spawnSound
  ) where

import           Control.Arrow           ((***))
import           Control.Monad           (filterM, join)
import           Control.Monad.IO.Class

import           Data.List               (find, partition)
import           Data.Maybe              (fromMaybe, isJust, listToMaybe)

import           DBus
import           DBus.Client
import qualified DBus.Introspection      as I

import           System.Directory        (findExecutable, readable, writable)
import           System.Exit
import           System.FilePath

import           XMonad.Core             (X, getXMonadDir)
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
  | IOTest (IO Bool)
  | DBusEndpoint
    { ddDbusBus       :: BusName
    , ddDbusSystem    :: Bool
    , ddDbusObject    :: ObjectPath
    , ddDbusInterface :: InterfaceName
    , ddDbusMember    :: DBusMember
    }
  | Systemd UnitType String

data Dependency a = SubFeature (Feature a a)
  | Dependency
  -- TODO when would this ever be false?
  { depRequired :: Bool
  , depData     :: DependencyData
  }

data Feature a b = Feature
  { ftrAction   :: a
  -- TODO add a 'default' action that will proceed in case of failure
  , ftrSilent   :: Bool
  -- TODO this should be a semigroup
  , ftrChildren :: [Dependency b]
  } | ConstFeature a

evalFeature :: Feature a b -> IO (MaybeExe a)
evalFeature (ConstFeature x) = return $ Installed x []
evalFeature Feature { ftrAction = a, ftrSilent = s, ftrChildren = c } = do
  c' <- concat <$> mapM go c
  return $ case foldl groupResult ([], []) c' of
    ([], opt)  -> Installed a opt
    (req, opt) -> if s then Ignore else Missing req opt
  where
    go (SubFeature Feature { ftrChildren = cs }) = concat <$> mapM go cs
    go (SubFeature (ConstFeature _)) = return []
    go Dependency { depRequired = r, depData = d } = do
      i <- depInstalled d
      return [(r, d) | not i ]
    groupResult (x, y) (True, z)  = (z:x, y)
    groupResult (x, y) (False, z) = (x, z:y)

exe :: String -> Dependency a
exe n = Dependency
  { depRequired = True
  , depData = Executable n
  }

unit :: UnitType -> String -> Dependency a
unit t n = Dependency
  { depRequired = True
  , depData = Systemd t n
  }

path :: Bool -> Bool -> String -> Dependency a
path r w n = Dependency
  { depRequired = True
  , depData = AccessiblePath n r w
  }

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
data MaybeExe a = Installed a [DependencyData]
  | Missing [DependencyData] [DependencyData]
  | Ignore
  deriving (Foldable, Traversable)

instance Functor MaybeExe where
  fmap f (Installed x ds)  = Installed (f x) ds
  fmap _ (Missing req opt) = Missing req opt
  fmap _ Ignore            = Ignore

type MaybeX = MaybeExe (X ())

exeInstalled :: String -> IO Bool
exeInstalled x = isJust <$> findExecutable x

unitInstalled :: UnitType -> String -> IO Bool
unitInstalled u x = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> True
    _           -> False
  where
    cmd = fmtCmd "systemctl" $ ["--user" | u == UserUnit] ++ ["status", x]

-- pathAccessible :: FilePath -> Bool -> Bool -> IO (Maybe String)
pathAccessible :: FilePath -> Bool -> Bool -> IO Bool
pathAccessible p testread testwrite = do
  res <- getPermissionsSafe p
  let msg = permMsg res
  return msg
  -- return $ fmap (\m -> m ++ ": " ++ p) msg
  where
    testPerm False _ _ = Nothing
    testPerm True f r  = Just $ f r
    -- permMsg NotFoundError            = Just "file not found"
    -- permMsg PermError                = Just "could not get permissions"
    permMsg NotFoundError            = False
    permMsg PermError                = False
    permMsg (PermResult r) =
      case (testPerm testread readable r, testPerm testwrite writable r) of
        -- (Just False, Just False) -> Just "file not readable or writable"
        -- (Just False, _)          -> Just "file not readable"
        -- (_, Just False)          -> Just "file not writable"
        -- _                        -> Nothing
        (Just True, Just True) -> True
        (Just True, Nothing)   -> True
        (Nothing, Just True)   -> True
        _                      -> False
        -- (Just False, Just False) -> Just "file not readable or writable"
        -- (Just False, _)          -> Just "file not readable"
        -- (_, Just False)          -> Just "file not writable"
        -- _                        -> Nothing

introspectInterface :: InterfaceName
introspectInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

introspectMethod :: MemberName
introspectMethod = memberName_ "Introspect"

dbusInstalled :: BusName -> Bool -> ObjectPath -> InterfaceName -> DBusMember
  -> IO Bool
dbusInstalled bus usesystem objpath iface mem = do
  client <- if usesystem then connectSystem else connectSession
  reply <- call_ client (methodCall objpath introspectInterface introspectMethod)
           { methodCallDestination = Just bus }
  let res = findMem =<< I.parseXML objpath =<< fromVariant
        =<< listToMaybe (methodReturnBody reply)
  disconnect client
  return $ fromMaybe False res
  where
    findMem obj = fmap (matchMem mem)
      $ find (\i -> I.interfaceName i == iface)
      $ I.objectInterfaces obj
    matchMem (Method_ n) = elem n . fmap I.methodName . I.interfaceMethods
    matchMem (Signal_ n) = elem n . fmap I.signalName . I.interfaceSignals
    matchMem (Property_ n) = elem n . fmap I.propertyName . I.interfaceProperties

-- TODO somehow get this to preserve error messages if something isn't found
depInstalled :: DependencyData -> IO Bool
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

checkInstalled :: [Dependency a] -> IO ([DependencyData], [DependencyData])
checkInstalled = fmap go . filterMissing
  where
    go = join (***) (fmap depData) . partition depRequired

filterMissing :: [Dependency a] -> IO [Dependency a]
filterMissing = filterM (fmap not . depInstalled . depData)

runIfInstalled :: [Dependency a] -> b -> IO (MaybeExe b)
runIfInstalled ds x = evalFeature $
  Feature
  { ftrAction = x
  , ftrSilent = False
  , ftrChildren = ds
  }

spawnIfInstalled :: MonadIO m => String -> IO (MaybeExe (m ()))
spawnIfInstalled n = runIfInstalled [exe n] $ spawn n

spawnCmdIfInstalled :: MonadIO m => String -> [String] -> IO (MaybeExe (m ()))
spawnCmdIfInstalled n args = runIfInstalled [exe n] $ spawnCmd n args

whenInstalled :: Monad m => MaybeExe (m ()) -> m ()
whenInstalled = flip ifInstalled skip

ifInstalled ::  MaybeExe a -> a -> a
ifInstalled (Installed x _) _ = x
ifInstalled _ alt             = alt

noCheck :: Monad m => a () -> m (MaybeExe (a ()))
noCheck = return . flip Installed []

-- not sure what to do with these

soundDir :: FilePath
soundDir = "sound"

spawnSound :: MonadIO m => FilePath -> m () -> m () -> IO (MaybeExe (m ()))
spawnSound file pre post = runIfInstalled [exe "paplay"]
  $ pre >> playSound file >> post

playSound :: MonadIO m => FilePath -> m ()
playSound file = do
  p <- (</> soundDir </> file) <$> getXMonadDir
  -- paplay seems to have less latency than aplay
  spawnCmd "paplay" [p]

partitionMissing :: [MaybeExe a] -> ([DependencyData], [DependencyData])
partitionMissing = foldl (\(a, b) -> ((a++) *** (b++)) . go) ([], [])
  where
    go (Installed _ opt) = ([], opt)
    go (Missing req opt) = (req, opt)
    go Ignore            = ([], [])

fmtMissing :: DependencyData -> String
-- TODO this error message is lame
fmtMissing (IOTest _) = "some random test failed"
fmtMissing DBusEndpoint {} = "some random dbus path is missing"
fmtMissing (AccessiblePath p True False) = "path '" ++ p ++ "' not readable"
fmtMissing (AccessiblePath p False True) = "path '" ++ p ++ "' not writable"
fmtMissing (AccessiblePath p True True) = "path '" ++ p ++ "' not readable/writable"
fmtMissing (AccessiblePath p _ _) = "path '" ++ p ++ "' not ...something"
fmtMissing (Executable n) = "executable '" ++ n ++ "' not found"
fmtMissing (Systemd st n) = "systemd " ++ unitType st ++ " unit '"
  ++ n ++ "' not found"
  where
    unitType SystemUnit = "system"
    unitType UserUnit   = "user"

fmtMsgs :: [DependencyData] -> [DependencyData] -> [String]
fmtMsgs req opt = ("[WARNING] "++)
  <$> (("[REQUIRED DEP] "++) . fmtMissing <$> req)
  ++ (("[OPTIONAL DEP] "++) . fmtMissing <$> opt)

warnMissing :: [MaybeExe a] -> IO ()
warnMissing = mapM_ putStrLn . uncurry fmtMsgs . partitionMissing

-- fmtType (AccessiblePath _ _ _) = undefined

-- splitDeps :: [MaybeExe a] -> ([a], [String])
-- splitDeps xs = undefined

-- splitDeps' :: [m (MaybeExe a)] -> ([m a], [String])
-- splitDeps' xs = undefined
