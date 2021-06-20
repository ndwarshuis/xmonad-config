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
import           System.Process

import           XMonad.Core
import           XMonad.Internal.Process     (readCreateProcessWithExitCode')
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

runScreenLock :: IOMaybeX
runScreenLock = spawnIfInstalled myScreenlock

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

withShellOutput :: Show a => String -> (String -> a) -> IO (Maybe a)
withShellOutput cmd f = do
  (rc, out, _) <- readCreateProcessWithExitCode' (shell cmd) ""
  return $ case rc of
    ExitSuccess -> Just $ f out
    _           -> Nothing

-- TODO this will work for most of my use cases but won't work in general
-- because it assumes "Intel" means "integrated graphics" ...sorry AMD
-- TODO this is hacky AF, I really only need the lspci command and the rest
-- can be parsed with some simple string matching if I use the -vmm option
hasSwitchableGPU :: IO (Maybe Bool)
hasSwitchableGPU = withShellOutput cmd hasIntelAndOther
  where
    cmd = fmtCmd "lspci" ["-mm"]
        #!| fmtCmd "grep" ["VGA"]
        #!| fmtCmd "sed" ["'s/ \"\\([^\"]*\\)\"*/|\\1/g'"]
        #!| fmtCmd "cut" ["-f3", "-d'|'"]
    hasIntelAndOther out =
      let vendors = lines out
          ivendors = filter (== "Intel Corporation") vendors in
      length vendors > length ivendors && not (null ivendors)

-- this is hacky but so much easier than the "pure haskell" solution
hasBattery :: IO (Maybe Bool)
hasBattery = withShellOutput (fmtCmd "cat" ["/sys/class/power_supply/*/type"])
  $ elem "Battery" . lines

requireOptimus :: IO Bool
requireOptimus = do
  s <- hasSwitchableGPU
  b <- hasBattery
  case (s, b) of
    (Just True, Just True) -> return True
    _                      -> warn >> return False
  where
    warn = putStrLn
      "WARNING: could not determine if switchable GPU present. Assuming not"

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
runOptimusPrompt = do
  g <- requireOptimus
  if g then runIfInstalled [exe myOptimusManager] runOptimusPrompt'
    else return Ignore

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
      Shutdown  -> (io runScreenLock >>= whenInstalled) >> runSuspend
      Hibernate -> (io runScreenLock >>= whenInstalled) >> runHibernate
      Reboot    -> runReboot
