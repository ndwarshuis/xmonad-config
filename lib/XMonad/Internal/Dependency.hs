--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeExe(..)
  , UnitType(..)
  , Dependency(..)
  , DependencyData(..)
  , MaybeX
  , exe
  , systemUnit
  , userUnit
  , pathR
  , pathW
  , pathRW
  , checkInstalled
  , createInstalled
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

import           Data.List               (partition)
import           Data.Maybe              (isJust)

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

data DependencyData = Executable String
  | AccessiblePath FilePath Bool Bool
  | Systemd UnitType String
  deriving (Eq, Show)

data Dependency = Dependency
  { depRequired :: Bool
  , depData     :: DependencyData
  }
  deriving (Eq, Show)

exe :: String -> Dependency
exe n = Dependency
  { depRequired = True
  , depData = Executable n
  }

unit :: UnitType -> String -> Dependency
unit t n = Dependency
  { depRequired = True
  , depData = Systemd t n
  }

path :: Bool -> Bool -> String -> Dependency
path r w n = Dependency
  { depRequired = True
  , depData = AccessiblePath n r w
  }

pathR :: String -> Dependency
pathR = path True False

pathW :: String -> Dependency
pathW = path False True

pathRW :: String -> Dependency
pathRW = path True True

systemUnit :: String -> Dependency
systemUnit = unit SystemUnit

userUnit :: String -> Dependency
userUnit = unit UserUnit

-- TODO this is poorly named. This actually represents an action that has
-- one or more dependencies (where "action" is not necessarily executing an exe)
data MaybeExe a = Installed a [DependencyData]
  | Missing [DependencyData] [DependencyData]
  | Ignore
  deriving (Eq, Show)

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

-- TODO somehow get this to preserve error messages if something isn't found
depInstalled :: DependencyData -> IO Bool
depInstalled (Executable n)         = exeInstalled n
depInstalled (Systemd t n)          = unitInstalled t n
depInstalled (AccessiblePath p r w) = pathAccessible p r w
    -- (AccessiblePath p r w) -> pathAccessible p r w

checkInstalled :: [Dependency] -> IO ([DependencyData], [DependencyData])
checkInstalled = fmap go . filterMissing
  where
    go = join (***) (fmap depData) . partition depRequired

createInstalled :: [DependencyData] -> [DependencyData] -> a -> MaybeExe a
createInstalled req opt x = if null req then Installed x opt else Missing req opt

filterMissing :: [Dependency] -> IO [Dependency]
filterMissing = filterM (fmap not . depInstalled . depData)

runIfInstalled :: MonadIO m => [Dependency] -> m a -> IO (MaybeExe (m a))
runIfInstalled ds x = do
  (req, opt) <- checkInstalled ds
  return $ createInstalled req opt x

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
