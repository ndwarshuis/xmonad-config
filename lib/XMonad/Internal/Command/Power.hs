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
  ) where

import           Control.Arrow               (first)

import qualified Data.Map                    as M

import           Graphics.X11.Types

import           System.Directory
import           System.Exit

import           XMonad.Core
import           XMonad.Internal.Shell
import qualified XMonad.Internal.Theme       as T
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt

--------------------------------------------------------------------------------
-- | Core commands

runScreenLock :: X ()
runScreenLock = spawn "screenlock"

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

runOptimusPrompt :: X ()
runOptimusPrompt = do
  nvidiaOn <- io isUsingNvidia
  switch $ if nvidiaOn then "intel" else "nvidia"
  where
    switch mode = confirmPrompt T.promptTheme (prompt mode) (cmd mode)
    prompt mode = "gpu switch to " ++ mode ++ "?"
    cmd mode = spawn $
      unwords ["optimus-manager", "--switch", mode, "--no-confirm"]
      #!&& "killall xmonad"

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

runPowerPrompt :: X ()
runPowerPrompt = mkXPrompt PowerPrompt theme comp executeAction
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
      Shutdown  -> runScreenLock >> runSuspend
      Hibernate -> runScreenLock >> runHibernate
      Reboot    -> runReboot
