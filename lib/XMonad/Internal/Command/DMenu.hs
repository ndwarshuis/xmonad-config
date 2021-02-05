--------------------------------------------------------------------------------
-- | Dmenu (Rofi) Commands

module XMonad.Internal.Command.DMenu
  ( runCmdMenu
  , runAppMenu
  , runClipMenu
  , runWinMenu
  , runNetMenu
  , runDevMenu
  , runBwMenu
  , runShowKeys
  , runAutorandrMenu
  ) where

import           Control.Monad.Reader

import           Graphics.X11.Types

import           System.IO

import           XMonad.Core              hiding (spawn)
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import           XMonad.Util.NamedActions

--------------------------------------------------------------------------------
-- | Other internal functions

myDmenuCmd :: String
myDmenuCmd = "rofi"

spawnDmenuCmd :: [String] -> X ()
spawnDmenuCmd = spawnCmd myDmenuCmd

themeArgs :: String -> [String]
themeArgs hexColor =
  [ "-theme-str"
  , "'#element.selected.normal { background-color: " ++ hexColor ++ "; }'"
  ]

myDmenuMatchingArgs :: [String]
myDmenuMatchingArgs = ["-i"] -- case insensitivity

--------------------------------------------------------------------------------
-- | Exported Commands

devSecrets :: [String]
devSecrets = concatMap (\x -> ["-s", x])
  [ "/tmp/media/ndwar/Roylab:user=ndwarshuis3@gatech.edu,host=outlook.office365.com"
  , "/tmp/media/ndwar/MC3M:user=ndwarshuis3@gatech.edu,host=outlook.office365.com"
  ] ++
  concatMap (\x -> ["-b", x])
  [ "/home/ndwar/.ssh:\"Veracrypt (ssh)\""
  , "/home/ndwar/.config/gnupg:\"Veracrypt (gpg)\""
  , "/tmp/media/ndwar/accounts:\"Veracrypt (accounts)\""
  , "/tmp/media/ndwar/ansible-pki:\"Veracrypt (Ansible PKI)\""
  , "/tmp/media/ndwar/call-logs:\"Veracrypt (ACR)\""
  ] ++
  concatMap (\x -> ["-v", x])
  [ "/tmp/media/ndwar/accounts:/mnt/data/Documents/personal_records/financial/acnt.crypt"
  , "/home/ndwar/.ssh:/mnt/data/Documents/crypt/ssh-config"
  , "/home/ndwar/.config/gnupg:/mnt/data/Documents/crypt/gnupg"
  , "/tmp/media/ndwar/ansible-pki:/home/ndwar/.ansible/openvpn.vcrypt"
  , "/tmp/media/ndwar/call-logs:/mnt/data/Documents/personal_records/call_logs"
  ]

runDevMenu :: X ()
runDevMenu = spawnCmd "rofi-dev" $ devSecrets ++ rofiArgs
  where
    rofiArgs = "--" : themeArgs "#999933" ++ myDmenuMatchingArgs

runBwMenu :: X ()
runBwMenu = spawnCmd "rofi-bw" $ ["-c", "--"] ++ themeArgs "#bb6600"
  ++ myDmenuMatchingArgs

runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys x = addName "Show Keybindings" $ do
  (h, _, _, _) <- io $ createProcess' $ (shell' cmd) { std_in = CreatePipe }
  io $ forM_ h $ \h' -> hPutStr h' (unlines $ showKm x) >> hClose h'
  where cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
          ++ themeArgs "#a200ff" ++ myDmenuMatchingArgs

runCmdMenu :: X ()
runCmdMenu = spawnDmenuCmd ["-show", "run"]

runAppMenu :: X ()
runAppMenu = spawnDmenuCmd ["-show", "drun"]

runClipMenu :: X ()
runClipMenu = spawnDmenuCmd $
  [ "-modi", "\"clipboard:greenclip print\""
  , "-show", "clipboard"
  , "-run-command", "'{cmd}'"
  ] ++ themeArgs "#00c44e"

runWinMenu :: X ()
runWinMenu = spawnDmenuCmd ["-show", "window"]

runNetMenu :: X ()
runNetMenu = spawnCmd "networkmanager_dmenu" $ themeArgs "#ff3333"

runAutorandrMenu :: X ()
runAutorandrMenu = spawnCmd "rofi-autorandr" $ themeArgs "#ff0066"
