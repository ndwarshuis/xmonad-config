--------------------------------------------------------------------------------
-- | Module for monitoring removable drive events
--
-- Currently, its only purpose is to play Super Mario sounds when a drive is
-- inserted or removed. Why? Because I can.

module XMonad.Internal.Concurrent.Removable (runRemovableMon) where

import           Control.Concurrent
import           Control.Monad

import           Data.Map.Lazy                (Map, member)

import           DBus
import           DBus.Client

import           XMonad.Internal.DBus.Control (pathExists)
import           XMonad.Internal.Shell

bus :: BusName
bus = busName_ "org.freedesktop.UDisks2"

path :: ObjectPath
path = objectPath_ "/org/freedesktop/UDisks2"

interface :: InterfaceName
interface = interfaceName_ "org.freedesktop.DBus.ObjectManager"

memAdded :: MemberName
memAdded = memberName_ "InterfacesAdded"

memRemoved :: MemberName
memRemoved = memberName_ "InterfacesRemoved"

driveInsertedSound :: FilePath
driveInsertedSound = "smb_powerup.wav"

driveRemovedSound :: FilePath
driveRemovedSound = "smb_pipe.wav"

ruleUdisks :: MatchRule
ruleUdisks = matchAny
  { matchPath = Just path
  , matchInterface = Just interface
  }

driveFlag :: String
driveFlag = "org.freedesktop.UDisks2.Drive"

addedHasDrive :: [Variant] -> Bool
addedHasDrive [_, a] = maybe False (member driveFlag)
  (fromVariant a :: Maybe (Map String (Map String Variant)))
addedHasDrive _ = False

removedHasDrive :: [Variant] -> Bool
removedHasDrive [_, a] = maybe False (driveFlag `elem`)
  (fromVariant a :: Maybe [String])
removedHasDrive _ = False

playSoundMaybe :: FilePath -> Bool -> IO ()
playSoundMaybe p b = when b $ playSound p

-- NOTE: the udisks2 service should be already running for this module to work.
-- If it not already, we won't see any signals from the dbus until it is
-- started (it will work after it is started however). It seems safe to simply
-- enable the udisks2 service at boot; however this is not default behavior.
listenDevices :: IO ()
listenDevices = do
  client <- connectSystem
  _ <- addMatch' client memAdded driveInsertedSound addedHasDrive
  _ <- addMatch' client memRemoved driveRemovedSound removedHasDrive
  forever (threadDelay 5000000)
  where
    addMatch' client m p f = addMatch client ruleUdisks { matchMember = Just m }
      $ playSoundMaybe p . f . signalBody

runRemovableMon :: IO ()
runRemovableMon = do
  e <- pathExists True bus path
  if e then listenDevices else
    putStrLn "WARNING: udisks not running. Super Mario disk sounds disabled."
