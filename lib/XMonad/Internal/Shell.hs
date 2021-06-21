--------------------------------------------------------------------------------
-- | Functions for formatting and spawning shell commands

module XMonad.Internal.Shell
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
  , spawnSound
  , playSound
  , doubleQuote
  , singleQuote
  , skip
  , (#!&&)
  , (#!||)
  , (#!|)
  , (#!>>)
  ) where

import           Control.Monad           (filterM)
import           Control.Monad.IO.Class

import           Data.Maybe              (isJust)

import           System.Directory        (findExecutable)
import           System.Exit
import           System.FilePath.Posix

import           XMonad.Core             (X, getXMonadDir)
import           XMonad.Internal.Process

--------------------------------------------------------------------------------
-- | Gracefully handling missing binaries

data UnitType = SystemUnit | UserUnit deriving (Eq, Show)

data DependencyType = Executable | Systemd UnitType deriving (Eq, Show)

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
  , depType = Executable }

unit :: UnitType -> String -> Dependency
unit t n = Dependency
  { depRequired = True
  , depName = n
  , depType = Systemd t }

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
    fmtType Executable = "executable"
    fmtType (Systemd u) =
      "systemd " ++ (if u == UserUnit then "user" else "system") ++ " unit"
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

depInstalled :: Dependency -> IO Bool
depInstalled Dependency { depName = n, depType = t } =
  case t of
    Executable -> exeInstalled n
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

skip :: Monad m => m ()
skip = return ()

noCheck :: Monad m => a () -> m (MaybeExe (a ()))
noCheck = return . flip Installed []

--------------------------------------------------------------------------------
-- | Opening subshell

spawnCmd :: MonadIO m => String -> [String] -> m ()
spawnCmd cmd args = spawn $ fmtCmd cmd args

--------------------------------------------------------------------------------
-- | Playing sound

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

--------------------------------------------------------------------------------
-- | Formatting commands

fmtCmd :: String -> [String] -> String
fmtCmd cmd args = unwords $ cmd : args

(#!&&) :: String -> String -> String
cmdA #!&& cmdB = cmdA ++ " && " ++ cmdB

infixr 0 #!&&

(#!|) :: String -> String -> String
cmdA #!| cmdB = cmdA ++ " | " ++ cmdB

infixr 0 #!|

(#!||) :: String -> String -> String
cmdA #!|| cmdB = cmdA ++ " || " ++ cmdB

infixr 0 #!||

(#!>>) :: String -> String -> String
cmdA #!>> cmdB = cmdA ++ "; " ++ cmdB

infixr 0 #!>>

doubleQuote :: String -> String
doubleQuote s = "\"" ++ s ++ "\""

singleQuote :: String -> String
singleQuote s = "'" ++ s ++ "'"
