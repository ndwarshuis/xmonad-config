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

import           System.Directory         (XdgDirectory (..), getXdgDirectory)
import           System.IO

import           XMonad.Core              hiding (spawn)
import           XMonad.Internal.Notify
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import           XMonad.Util.NamedActions

--------------------------------------------------------------------------------
-- | DMenu executables

myDmenuCmd :: String
myDmenuCmd = "rofi"

myDmenuDevices :: String
myDmenuDevices = "rofi-dev"

myDmenuPasswords :: String
myDmenuPasswords = "rofi-bw"

myDmenuMonitors :: String
myDmenuMonitors = "rofi-autorandr"

myDmenuNetworks :: String
myDmenuNetworks = "networkmanager_dmenu"

--------------------------------------------------------------------------------
-- | Other internal functions

spawnDmenuCmd :: [String] -> IO MaybeX
spawnDmenuCmd = spawnCmdIfInstalled myDmenuCmd

themeArgs :: String -> [String]
themeArgs hexColor =
  [ "-theme-str"
  , "'#element.selected.normal { background-color: " ++ hexColor ++ "; }'"
  ]

myDmenuMatchingArgs :: [String]
myDmenuMatchingArgs = ["-i"] -- case insensitivity

--------------------------------------------------------------------------------
-- | Exported Commands

runDevMenu :: IO MaybeX
runDevMenu = runIfInstalled [exe myDmenuDevices] $ do
  c <- io $ getXdgDirectory XdgConfig "rofi/devices.yml"
  spawnCmd myDmenuDevices
    $ ["-c", c]
    ++ "--" : themeArgs "#999933"
    ++ myDmenuMatchingArgs

runBwMenu :: IO MaybeX
runBwMenu = runIfInstalled [exe myDmenuPasswords] $
  spawnCmd myDmenuPasswords $ ["-c"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs

runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys x = addName "Show Keybindings" $ do
  s <- io $ runDMenuShowKeys x
  ifInstalled s
    $ spawnNotify
    $ defNoteError { body = Just $ Text "could not display keymap" }

runDMenuShowKeys :: [((KeyMask, KeySym), NamedAction)] -> IO MaybeX
runDMenuShowKeys kbs = runIfInstalled [exe myDmenuCmd] $ io $ do
  (h, _, _, _) <- createProcess' $ (shell' cmd) { std_in = CreatePipe }
  forM_ h $ \h' -> hPutStr h' (unlines $ showKm kbs) >> hClose h'
  where
    cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
      ++ themeArgs "#3399FF" ++ myDmenuMatchingArgs

runCmdMenu :: IO MaybeX
runCmdMenu = spawnDmenuCmd ["-show", "run"]

runAppMenu :: IO MaybeX
runAppMenu = spawnDmenuCmd ["-show", "drun"]

runClipMenu :: IO MaybeX
runClipMenu = runIfInstalled [exe myDmenuCmd, exe "greenclip"]
  $ spawnCmd myDmenuCmd args
  where
    args = [ "-modi", "\"clipboard:greenclip print\""
           , "-show", "clipboard"
           , "-run-command", "'{cmd}'"
           ] ++ themeArgs "#00c44e"

runWinMenu :: IO MaybeX
runWinMenu = spawnDmenuCmd ["-show", "window"]

runNetMenu :: IO MaybeX
runNetMenu = spawnCmdIfInstalled myDmenuNetworks $ themeArgs "#ff3333"

runAutorandrMenu :: IO MaybeX
runAutorandrMenu = spawnCmdIfInstalled myDmenuMonitors $ themeArgs "#ff0066"
