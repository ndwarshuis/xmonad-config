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
import           XMonad.Internal.Shell
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

runScreenLock :: Feature (X ())
runScreenLock = featureExe "screen locker" myScreenlock

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

-- TODO doesn't this need to also lock the screen?
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

runOptimusPrompt :: FeatureX
runOptimusPrompt = featureDefault "graphics switcher" [Executable myOptimusManager]
  runOptimusPrompt'

--------------------------------------------------------------------------------
-- | Universal power prompt

data PowerMaybeAction = Poweroff
    | Shutdown
    | Hibernate
    | Reboot
    deriving (Eq)

instance Enum PowerMaybeAction where
  toEnum 0 = Poweroff
  toEnum 1 = Shutdown
  toEnum 2 = Hibernate
  toEnum 3 = Reboot
  toEnum _ = errorWithoutStackTrace "Main.Enum.PowerMaybeAction.toEnum: bad argument"

  fromEnum Poweroff  = 0
  fromEnum Shutdown  = 1
  fromEnum Hibernate = 2
  fromEnum Reboot    = 3

data PowerPrompt = PowerPrompt

instance XPrompt PowerPrompt where
    showXPrompt PowerPrompt = "(P)oweroff (S)uspend (H)ibernate (R)eboot:"

runPowerPrompt :: X () -> X ()
runPowerPrompt lock = mkXPrompt PowerPrompt theme comp executeMaybeAction
  where
    comp = mkComplFunFromList []
    theme = T.promptTheme { promptKeymap = keymap }
    keymap = M.fromList
      $ ((controlMask, xK_g), quit) :
      map (first $ (,) 0)
      [ (xK_p, sendMaybeAction Poweroff)
      , (xK_s, sendMaybeAction Shutdown)
      , (xK_h, sendMaybeAction Hibernate)
      , (xK_r, sendMaybeAction Reboot)
      , (xK_Return, quit)
      , (xK_Escape, quit)
      ]
    sendMaybeAction a = setInput (show $ fromEnum a) >> setSuccess True >> setDone True
    executeMaybeAction a = case toEnum $ read a of
      Poweroff  -> runPowerOff
      Shutdown  -> lock >> runSuspend
      Hibernate -> lock >> runHibernate
      Reboot    -> runReboot
