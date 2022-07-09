--------------------------------------------------------------------------------
-- | Module for monitoring removable drive events
--
-- Currently, its only purpose is to play Super Mario sounds when a drive is
-- inserted or removed. Why? Because I can.

module XMonad.Internal.DBus.Removable (runRemovableMon) where

import           Control.Monad

import           Data.Internal.DBus
import           Data.Internal.Dependency
import           Data.Map.Strict                 (Map, member)

import           DBus
import           DBus.Client

import           XMonad.Core                     (io)
import           XMonad.Internal.Command.Desktop

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

dbusDep :: MemberName -> DBusDependency_ SysClient
dbusDep m = Endpoint [Package Official "udisks2"] bus path interface $ Signal_ m

addedDep :: DBusDependency_ SysClient
addedDep = dbusDep memAdded

removedDep :: DBusDependency_ SysClient
removedDep = dbusDep memRemoved

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
playSoundMaybe p b = when b $ io $ playSound p

-- NOTE: the udisks2 service should be already running for this module to work.
-- If it not already, we won't see any signals from the dbus until it is
-- started (it will work after it is started however). It seems safe to simply
-- enable the udisks2 service at boot; however this is not default behavior.
listenDevices :: SysClient -> IO ()
listenDevices cl = do
  addMatch' memAdded driveInsertedSound addedHasDrive
  addMatch' memRemoved driveRemovedSound removedHasDrive
  where
    addMatch' m p f = void $ addMatch (toClient cl) ruleUdisks { matchMember = Just m }
      $ playSoundMaybe p . f . signalBody

runRemovableMon :: Maybe SysClient -> SometimesIO
runRemovableMon cl =
  sometimesDBus cl "removeable device monitor" "dbus monitor" deps listenDevices
  where
    deps = toAnd_ addedDep removedDep
