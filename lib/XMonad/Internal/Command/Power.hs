--------------------------------------------------------------------------------
-- | Commands for controlling power

module XMonad.Internal.Command.Power
  -- commands
  ( runHibernate
  , runOptimusPrompt
  , runPowerOff
  , runPowerPrompt
  , runReboot
  , runScreenLock
  , runSuspend
  , runSuspendPrompt
  , runQuitPrompt

  -- daemons
  , runAutolock

  -- functions
  , hasBattery
  , suspendPrompt
  , quitPrompt
  , powerPrompt
  ) where

import           Control.Arrow               (first)

import           Data.Either
import qualified Data.Map                    as M

import           Graphics.X11.Types

import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO.Error
import           System.Process              (ProcessHandle)

import           XMonad.Core
import           XMonad.Internal.Dependency
import           XMonad.Internal.Process     (spawnPipeArgs)
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

myPrimeOffload :: String
myPrimeOffload = "prime-offload"

--------------------------------------------------------------------------------
-- | Core commands

runScreenLock :: SometimesX
runScreenLock = sometimesExe "screen locker" "i3lock script" False myScreenlock

runPowerOff :: X ()
runPowerOff = spawn "systemctl poweroff"

runSuspend :: X ()
runSuspend = spawn "systemctl suspend"

runHibernate :: X ()
runHibernate = spawn "systemctl hibernate"

runReboot :: X ()
runReboot = spawn "systemctl reboot"

--------------------------------------------------------------------------------
-- | Autolock

runAutolock :: Sometimes (IO ProcessHandle)
runAutolock = sometimesIO_ "automatic screen lock" "xss-lock" tree cmd
  where
    tree = And_ (Only_ $ sysExe "xss-lock") (Only_ $ IOSometimes_ runScreenLock)
    cmd = snd <$> spawnPipeArgs "xss-lock" ["--ignore-sleep", "screenlock"]

--------------------------------------------------------------------------------
-- | Confirmation prompts

confirmPrompt' :: String -> X () -> T.FontBuilder -> X ()
confirmPrompt' s x fb = confirmPrompt (T.promptTheme fb) s x

suspendPrompt :: T.FontBuilder -> X ()
suspendPrompt = confirmPrompt' "suspend?" runSuspend

quitPrompt :: T.FontBuilder -> X ()
quitPrompt = confirmPrompt' "quit?" $ io exitSuccess

sometimesPrompt :: String -> (T.FontBuilder -> X ()) -> SometimesX
sometimesPrompt n = sometimesIO n (n ++ " command") $ fontTreeAlt T.defFontFamily

-- TODO doesn't this need to also lock the screen?
runSuspendPrompt :: SometimesX
runSuspendPrompt = sometimesPrompt "suspend prompt" suspendPrompt

runQuitPrompt :: SometimesX
runQuitPrompt = sometimesPrompt "quit prompt" quitPrompt

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

runOptimusPrompt' :: T.FontBuilder -> X ()
runOptimusPrompt' fb = do
  nvidiaOn <- io isUsingNvidia
  switch $ if nvidiaOn then "integrated" else "nvidia"
  where
    switch mode = confirmPrompt' (prompt mode) (cmd mode) fb
    prompt mode = "gpu switch to " ++ mode ++ "?"
    cmd mode = spawn $
      myPrimeOffload
      #!&& unwords [myOptimusManager, "--switch", mode, "--no-confirm"]
      #!&& "killall xmonad"

runOptimusPrompt :: SometimesX
runOptimusPrompt = Sometimes "graphics switcher" [s]
  where
    s = Subfeature { sfData = r, sfName = "optimus manager", sfLevel = Error }
    r = IORoot runOptimusPrompt' t
    t = And1 (fontTreeAlt T.defFontFamily)
      $ listToAnds (socketExists "optimus-manager" socketName) $ sysExe
      <$> [myOptimusManager, myPrimeOffload]
    socketName = (</> "optimus-manager") <$> getTemporaryDirectory

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

runPowerPrompt :: SometimesX
runPowerPrompt = Sometimes "power prompt" [sf]
  where
    sf = Subfeature withLock "prompt with lock" Error
    withLock = IORoot (uncurry powerPrompt) tree
    tree = And12 (,) lockTree (fontTreeAlt T.defFontFamily)
    lockTree = Or (Only $ IOSometimes runScreenLock id) (Only $ IOConst skip)

powerPrompt :: X () -> T.FontBuilder -> X ()
powerPrompt lock fb = mkXPrompt PowerPrompt theme comp executeMaybeAction
  where
    comp = mkComplFunFromList theme []
    theme = (T.promptTheme fb) { promptKeymap = keymap }
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
