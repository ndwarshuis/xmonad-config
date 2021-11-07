--------------------------------------------------------------------------------
-- | Functions for handling dependencies

module XMonad.Internal.Dependency
  ( MaybeExe(..)
  , UnitType(..)
  , Dependency(..)
  , MaybeX
  , exe
  , systemUnit
  , userUnit
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

import           Control.Monad           (filterM)
import           Control.Monad.IO.Class

import           Data.Maybe              (isJust)

-- import           System.Directory        (findExecutable, readable, writable)
import           System.Directory        (findExecutable)
import           System.Exit
import           System.FilePath

import           XMonad.Core             (X, getXMonadDir)
-- import           XMonad.Internal.IO
import           XMonad.Internal.Process
import           XMonad.Internal.Shell

--------------------------------------------------------------------------------
-- | Gracefully handling missing binaries

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

data DependencyType = Executable
  -- | AccessiblePath FilePath Bool Bool
  | Systemd UnitType deriving (Eq, Show)

data Dependency = Dependency
  { depRequired :: Bool
  , depName     :: String
  , depType     :: DependencyType
  }
  deriving (Eq, Show)

exe :: String -> Dependency
exe n = Dependency
  { depRequired = True
  , depName = n
  , depType = Executable
  }

unit :: UnitType -> String -> Dependency
unit t n = Dependency
  { depRequired = True
  , depName = n
  , depType = Systemd t
  }

systemUnit :: String -> Dependency
systemUnit = unit SystemUnit

userUnit :: String -> Dependency
userUnit = unit UserUnit

data MaybeExe a = Installed a [Dependency] | Missing [Dependency] | Ignore

instance Functor MaybeExe where
  fmap f (Installed x ds) = Installed (f x) ds
  fmap _ (Missing x)      = Missing x
  fmap _ Ignore           = Ignore

type MaybeX = MaybeExe (X ())

warnMissing :: Dependency -> IO ()
warnMissing Dependency {depRequired = r, depName = n, depType = t } =
  putStrLn $ "WARNING: " ++ r' ++ " " ++ fmtType t ++ " not found: " ++ n
  where
    fmtType Executable           = "executable"
    -- fmtType (AccessiblePath _ _ _) = undefined
    fmtType (Systemd UserUnit)   = "systemd user unit"
    fmtType (Systemd SystemUnit) = "systemd system unit"
    r' = if r then "required" else "optional"

exeInstalled :: String -> IO Bool
exeInstalled x = isJust <$> findExecutable x

unitInstalled :: String -> UnitType -> IO Bool
unitInstalled x u = do
  (rc, _, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> True
    _           -> False
  where
    cmd = fmtCmd "systemctl" $ ["--user" | u == UserUnit] ++ ["status", x]

-- pathAccessible :: FilePath -> Bool -> Bool -> IO (Maybe String)
-- pathAccessible p testread testwrite = do
--   res <- getPermissionsSafe p
--   let msg = permMsg res
--   return $ fmap (\m -> m ++ ": " ++ p) msg
--   where
--     testPerm False _ _ = Nothing
--     testPerm True f r  = Just $ f r
--     permMsg NotFoundError            = Just "file not found"
--     permMsg PermError                = Just "could not get permissions"
--     permMsg (PermResult r) =
--       case (testPerm testread readable r, testPerm testwrite writable r) of
--         (Just False, Just False) -> Just "file not readable or writable"
--         (Just False, _)          -> Just "file not readable"
--         (_, Just False)          -> Just "file not writable"
--         _                        -> Nothing

depInstalled :: Dependency -> IO Bool
depInstalled Dependency { depName = n, depType = t } =
  case t of
    Executable -> exeInstalled n
    -- (AccessiblePath p r w) -> pathAccessible p r w
    Systemd u  -> unitInstalled n u

filterMissing :: [Dependency] -> IO [Dependency]
filterMissing = filterM (fmap not . depInstalled)

runIfInstalled :: MonadIO m => [Dependency] -> m () -> IO (MaybeExe (m ()))
runIfInstalled ds x = do
  missing <- filterMissing ds
  return $ if not $ any depRequired missing
    then Installed x $ filter (not . depRequired) missing
    else Missing missing

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
  path <- (</> soundDir </> file) <$> getXMonadDir
  -- paplay seems to have less latency than aplay
  spawnCmd "paplay" [path]
