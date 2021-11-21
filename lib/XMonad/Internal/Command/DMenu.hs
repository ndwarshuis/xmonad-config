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

import           System.Directory           (XdgDirectory (..), getXdgDirectory)
import           System.IO

import           XMonad.Core                hiding (spawn)
import           XMonad.Internal.Dependency
import           XMonad.Internal.Notify
import           XMonad.Internal.Process
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

spawnDmenuCmd :: String -> [String] -> FeatureX
spawnDmenuCmd n = featureSpawnCmd n myDmenuCmd

themeArgs :: String -> [String]
themeArgs hexColor =
  [ "-theme-str"
  , "'#element.selected.normal { background-color: " ++ hexColor ++ "; }'"
  ]

myDmenuMatchingArgs :: [String]
myDmenuMatchingArgs = ["-i"] -- case insensitivity

--------------------------------------------------------------------------------
-- | Exported Commands

runDevMenu :: FeatureX
runDevMenu = featureRun "device manager" [Executable myDmenuDevices] $ do
  c <- io $ getXdgDirectory XdgConfig "rofi/devices.yml"
  spawnCmd myDmenuDevices
    $ ["-c", c]
    ++ "--" : themeArgs "#999933"
    ++ myDmenuMatchingArgs

runBwMenu :: FeatureX
runBwMenu = featureRun "password manager" [Executable myDmenuPasswords] $
  spawnCmd myDmenuPasswords $ ["-c"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs

-- TODO this is weirdly inverted
runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys x = addName "Show Keybindings" $ do
  s <- io $ evalFeature $ runDMenuShowKeys x
  ifInstalled s
    $ spawnNotify
    $ defNoteError { body = Just $ Text "could not display keymap" }

runDMenuShowKeys :: [((KeyMask, KeySym), NamedAction)] -> FeatureX
runDMenuShowKeys kbs =
  featureRun "keyboard shortcut menu" [Executable myDmenuCmd] $ io $ do
  (h, _, _, _) <- createProcess' $ (shell' cmd) { std_in = CreatePipe }
  forM_ h $ \h' -> hPutStr h' (unlines $ showKm kbs) >> hClose h'
  where
    cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
      ++ themeArgs "#7f66ff" ++ myDmenuMatchingArgs

runCmdMenu :: FeatureX
runCmdMenu = spawnDmenuCmd "command menu" ["-show", "run"]

runAppMenu :: FeatureX
runAppMenu = spawnDmenuCmd "app launcher" ["-show", "drun"]

runClipMenu :: FeatureX
runClipMenu =
  featureRun "clipboard manager" [Executable myDmenuCmd, Executable "greenclip"]
  $ spawnCmd myDmenuCmd args
  where
    args = [ "-modi", "\"clipboard:greenclip print\""
           , "-show", "clipboard"
           , "-run-command", "'{cmd}'"
           ] ++ themeArgs "#00c44e"

runWinMenu :: FeatureX
runWinMenu = spawnDmenuCmd "window switcher" ["-show", "window"]

runNetMenu :: FeatureX
runNetMenu =
  featureSpawnCmd "network control menu" myDmenuNetworks $ themeArgs "#ff3333"

runAutorandrMenu :: FeatureX
runAutorandrMenu =
  featureSpawnCmd "autorandr menu" myDmenuMonitors $ themeArgs "#ff0066"
