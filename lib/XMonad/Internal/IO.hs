{-# LANGUAGE ViewPatterns #-}

--------------------------------------------------------------------------------
-- | Random IO-ish functions used throughtout xmonad
--
-- Most (probably all) of these functions are intended to work with sysfs where
-- some safe assumptions can be made about file contents.

module XMonad.Internal.IO
  ( readInt
  , readBool
  , readPercent
  , writeInt
  , writeBool
  , writePercent
  , writePercentMin
  , writePercentMax
  , decPercent
  , incPercent
  -- , isReadable
  -- , isWritable
  , PermResult(..)
  , getPermissionsSafe
  ) where

import           Data.Char
import           Data.Text        (pack, unpack)
import           Data.Text.IO     as T (readFile, writeFile)

import           System.Directory
import           System.IO.Error

--------------------------------------------------------------------------------
-- | read

readInt :: (Read a, Integral a) => FilePath -> IO a
readInt = fmap (read . takeWhile isDigit . unpack) . T.readFile

readBool :: FilePath -> IO Bool
readBool = fmap (==(1 :: Int)) . readInt

--------------------------------------------------------------------------------
-- | write

writeInt :: (Show a, Integral a) => FilePath -> a -> IO ()
writeInt f = T.writeFile f . pack . show

writeBool :: FilePath -> Bool -> IO ()
writeBool f b = writeInt f ((if b then 1 else 0) :: Int)

--------------------------------------------------------------------------------
-- | percent-based read/write
--
-- "Raw" values are whatever is stored in sysfs and "percent" is the user-facing
-- value. Assume that the file being read has a min of 0 and an unchanging max
-- given by a runtime argument, which is scaled linearly to the range 0-100
-- (percent).

rawToPercent :: (Integral a, Integral b, Read b, RealFrac c) => (a, a) -> b -> c
rawToPercent (lower, upper) raw =
  100 * (fromIntegral raw - fromIntegral lower) / fromIntegral (upper - lower)
-- rawToPercent upper raw = 100 * fromIntegral raw / fromIntegral upper

readPercent :: (Integral a, RealFrac b) => (a, a) -> FilePath -> IO b
readPercent bounds path = do
  i <- readInt path
  return $ rawToPercent bounds (i :: Integer)

percentToRaw :: (Integral a, RealFrac b, Integral c) => (a, a) -> b -> c
percentToRaw (lower, upper) perc = round $
  fromIntegral lower + perc / 100.0 * (fromIntegral upper - fromIntegral lower)

writePercent :: (Integral a, RealFrac b) => (a, a) -> FilePath -> b -> IO b
writePercent bounds path perc = do
  let t | perc > 100 = 100
        | perc < 0 = 0
        | otherwise = perc
  writeInt path (percentToRaw bounds t :: Int)
  return t

writePercentMin :: (Integral a, RealFrac b) => (a, a) -> FilePath -> IO b
writePercentMin bounds path = writePercent bounds path 0

writePercentMax :: (Integral a, RealFrac b) => (a, a) -> FilePath -> IO b
writePercentMax bounds path = writePercent bounds path 100

shiftPercent :: (Integral a, RealFrac b) => (b -> b -> b) -> Int -> FilePath
  -> (a, a) -> IO b
shiftPercent f steps path bounds = writePercent bounds path . f stepsize
    =<< readPercent bounds path
  where
    stepsize = 100 / fromIntegral steps

incPercent :: (Integral a, RealFrac b) => Int -> FilePath -> (a, a) -> IO b
incPercent = shiftPercent (+)

decPercent :: (Integral a, RealFrac b) => Int -> FilePath -> (a, a) -> IO b
decPercent = shiftPercent subtract -- silly (-) operator thingy error

--------------------------------------------------------------------------------
-- | permission query

data PermResult a = PermResult a | NotFoundError | PermError
  deriving (Show, Eq)

-- instance Functor PermResult where
--   fmap f (PermResult r) = PermResult $ f r
--   fmap _ NotFoundError  = NotFoundError
--   fmap _ PermError      = PermError

getPermissionsSafe :: FilePath -> IO (PermResult Permissions)
getPermissionsSafe f = do
  r <- tryIOError $ getPermissions f
  return $ case r of
    Right z                            -> PermResult z
    Left (isPermissionError -> True)   -> PermError
    Left (isDoesNotExistError -> True) -> NotFoundError
    -- the above error should be the only ones thrown by getPermission,
    -- so the catchall case should never happen
    _                                  -> error "Unknown permission error"

-- isReadable :: FilePath -> IO (PermResult Bool)
-- isReadable = fmap (fmap readable) . getPermissionsSafe

-- isWritable :: FilePath -> IO (PermResult Bool)
-- isWritable = fmap (fmap writable) . getPermissionsSafe
