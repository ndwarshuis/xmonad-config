--------------------------------------------------------------------------------
-- | Common internal DBus functions

module XMonad.Internal.DBus.Common
  ( addMatchCallback
  , getDBusClient
  , withDBusClient
  , withDBusClient_
  , xmonadBusName
  ) where

import           Control.Exception
import           Control.Monad

import           DBus
import           DBus.Client

xmonadBusName :: BusName
xmonadBusName = busName_ "org.xmonad"

-- | Bind a callback to a signal match rule
addMatchCallback :: MatchRule -> ([Variant] -> IO ()) -> Client -> IO ()
addMatchCallback rule cb client = void $ addMatch client rule $ cb . signalBody

getDBusClient :: Bool -> IO (Maybe Client)
getDBusClient sys = do
  res <- try $ if sys then connectSystem else connectSession
  case res of
    Left e  -> putStrLn (clientErrorMessage e) >> return Nothing
    Right c -> return $ Just c

withDBusClient :: Bool -> (Client -> a) -> IO (Maybe a)
withDBusClient sys f = do
  client <- getDBusClient sys
  let r = f <$> client
  mapM_ disconnect client
  return r

withDBusClient_ :: Bool -> (Client -> IO ()) -> IO ()
withDBusClient_ sys f = do
  client <- getDBusClient sys
  mapM_ f client
  mapM_ disconnect client
