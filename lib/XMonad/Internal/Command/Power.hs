--------------------------------------------------------------------------------
-- | Commands for controlling power

module XMonad.Internal.Command.Power
  ( runHibernate
  , runOptimusPrompt
  , runPowerOff
  , runPowerPrompt
  , runReboot
  , runScreenLock
  , runSuspend
  , runSuspendPrompt
  , runQuitPrompt
  , hasBattery
  ) where

import           Control.Arrow               (first)

import           Data.Either
import qualified Data.Map                    as M

import           Graphics.X11.Types

import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO.Error

import           XMonad.Core
import           XMonad.Internal.Dependency
import qualified XMonad.Internal.Theme       as T
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt

--------------------------------------------------------------------------------
-- | Executables

myScreenlock :: String
myScreenlock = "screenlock"

myOptimusManager :: String
myOptimusManager = "optimus-manager"

--------------------------------------------------------------------------------
-- | Core commands

-- runScreenLock :: IO MaybeX
-- runScreenLock = spawnIfInstalled myScreenlock

runScreenLock :: Feature (X ()) (X ())
runScreenLock = Feature
  { ftrAction = spawn myScreenlock
  , ftrSilent = False
  , ftrChildren = [exe myScreenlock]
  }

runPowerOff :: X ()
runPowerOff = spawn "systemctl poweroff"

runSuspend :: X ()
runSuspend = spawn "systemctl suspend"

runHibernate :: X ()
runHibernate = spawn "systemctl hibernate"

runReboot :: X ()
runReboot = spawn "systemctl reboot"

--------------------------------------------------------------------------------
-- | Confirm prompt wrappers

runSuspendPrompt :: X ()
runSuspendPrompt = confirmPrompt T.promptTheme "suspend?" runSuspend

runQuitPrompt :: X ()
runQuitPrompt = confirmPrompt T.promptTheme "quit?" $ io exitSuccess

--------------------------------------------------------------------------------
-- | Nvidia Optimus

-- TODO for some reason the screen never wakes up after suspend when
-- the nvidia card is up, so block suspend if nvidia card is running
-- and warn user
isUsingNvidia :: IO Bool
isUsingNvidia = doesDirectoryExist "/sys/module/nvidia"

hasBattery :: IO (Maybe String)
hasBattery = do
  ps <- fromRight [] <$> tryIOError (listDirectory syspath)
  ts <- mapM readType ps
  -- TODO this is obviously stupid
  return $ if "Battery\n" `elem` ts then Nothing else Just "battery not found"
  where
    readType p = fromRight [] <$> tryIOError (readFile $ syspath </> p </> "type")
    syspath = "/sys/class/power_supply"

runOptimusPrompt' :: X ()
runOptimusPrompt' = do
  nvidiaOn <- io isUsingNvidia
  switch $ if nvidiaOn then "integrated" else "nvidia"
  where
    switch mode = confirmPrompt T.promptTheme (prompt mode) (cmd mode)
    prompt mode = "gpu switch to " ++ mode ++ "?"
    cmd mode = spawn $
      "prime-offload"
      #!&& unwords [myOptimusManager, "--switch", mode, "--no-confirm"]
      #!&& "killall xmonad"

runOptimusPrompt :: IO MaybeX
runOptimusPrompt = runIfInstalled [exe myOptimusManager] runOptimusPrompt'
-- runOptimusPrompt :: Feature (X ()) (X ())
-- runOptimusPrompt = Feature
--   { ftrAction = runOptimusPrompt'
--   , ftrSilent = False
--   , ftrChildren = [exe myOptimusManager]
--   }

--------------------------------------------------------------------------------
-- | Universal power prompt

data PowerAction = Poweroff
    | Shutdown
    | Hibernate
    | Reboot
    deriving (Eq)

instance Enum PowerAction where
  toEnum 0 = Poweroff
  toEnum 1 = Shutdown
  toEnum 2 = Hibernate
  toEnum 3 = Reboot
  toEnum _ = errorWithoutStackTrace "Main.Enum.PowerAction.toEnum: bad argument"

  fromEnum Poweroff  = 0
  fromEnum Shutdown  = 1
  fromEnum Hibernate = 2
  fromEnum Reboot    = 3

data PowerPrompt = PowerPrompt

instance XPrompt PowerPrompt where
    showXPrompt PowerPrompt = "(P)oweroff (S)uspend (H)ibernate (R)eboot:"

runPowerPrompt :: X () -> X ()
runPowerPrompt lock = mkXPrompt PowerPrompt theme comp executeAction
  where
    comp = mkComplFunFromList []
    theme = T.promptTheme { promptKeymap = keymap }
    keymap = M.fromList
      $ ((controlMask, xK_g), quit) :
      map (first $ (,) 0)
      [ (xK_p, sendAction Poweroff)
      , (xK_s, sendAction Shutdown)
      , (xK_h, sendAction Hibernate)
      , (xK_r, sendAction Reboot)
      , (xK_Return, quit)
      , (xK_Escape, quit)
      ]
    sendAction a = setInput (show $ fromEnum a) >> setSuccess True >> setDone True
    executeAction a = case toEnum $ read a of
      Poweroff  -> runPowerOff
      -- TODO these dependency functions need to be assembled elsewhere and fed
      -- to this function
      -- Shutdown  -> (io runScreenLock >>= whenInstalled) >> runSuspend
      -- Hibernate -> (io runScreenLock >>= whenInstalled) >> runHibernate
      Shutdown  -> lock >> runSuspend
      Hibernate -> lock >> runHibernate
      Reboot    -> runReboot

-- runPowerPrompt :: Feature (X ()) (X ()) -> IO (X ())
-- runPowerPrompt lock = do
--   lock' <- evalFeature lock
--   return $ mkXPrompt PowerPrompt theme comp $ executeAction $ fromRight (return ()) lock'
--   where
--     comp = mkComplFunFromList []
--     theme = T.promptTheme { promptKeymap = keymap }
--     keymap = M.fromList
--       $ ((controlMask, xK_g), quit) :
--       map (first $ (,) 0)
--       [ (xK_p, sendAction Poweroff)
--       , (xK_s, sendAction Shutdown)
--       , (xK_h, sendAction Hibernate)
--       , (xK_r, sendAction Reboot)
--       , (xK_Return, quit)
--       , (xK_Escape, quit)
--       ]
--     sendAction a = setInput (show $ fromEnum a) >> setSuccess True >> setDone True
--     executeAction l a = case toEnum $ read a of
--       Poweroff  -> runPowerOff
--       -- TODO these dependency functions need to be assembled elsewhere and fed
--       -- to this function
--       Shutdown  -> l >> runSuspend
--       Hibernate -> l >> runHibernate
--       Reboot    -> runReboot
