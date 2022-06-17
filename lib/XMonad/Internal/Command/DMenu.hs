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
  , runVPNMenu
  , runBTMenu
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

myDmenuBluetooth :: String
myDmenuBluetooth = "rofi-bt"

myDmenuVPN :: String
myDmenuVPN = "rofi-evpn"

myDmenuMonitors :: String
myDmenuMonitors = "rofi-autorandr"

myDmenuNetworks :: String
myDmenuNetworks = "networkmanager_dmenu"

--------------------------------------------------------------------------------
-- | Other internal functions

spawnDmenuCmd :: String -> [String] -> FeatureX
spawnDmenuCmd n = featureExeArgs n myDmenuCmd

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
runDevMenu = featureDefault "device manager" (Only $ exe myDmenuDevices) $ do
  c <- io $ getXdgDirectory XdgConfig "rofi/devices.yml"
  spawnCmd myDmenuDevices
    $ ["-c", c]
    ++ "--" : themeArgs "#999933"
    ++ myDmenuMatchingArgs

runBTMenu :: FeatureX
runBTMenu = featureExeArgs "bluetooth selector" myDmenuBluetooth
  $ "-c":themeArgs "#0044bb"

runBwMenu :: FeatureX
runBwMenu = featureDefault "password manager" (Only $ exe myDmenuPasswords) $
  spawnCmd myDmenuPasswords $ ["-c"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs

runVPNMenu :: FeatureX
runVPNMenu = featureDefault "VPN selector" (Only $ exe myDmenuVPN) $
  spawnCmd myDmenuVPN $ ["-c"] ++ themeArgs "#007766" ++ myDmenuMatchingArgs

-- TODO this is weirdly inverted
runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys x = addName "Show Keybindings" $ do
  s <- io $ evalFeature $ runDMenuShowKeys x
  ifSatisfied s
    $ spawnNotify
    $ defNoteError { body = Just $ Text "could not display keymap" }

runDMenuShowKeys :: [((KeyMask, KeySym), NamedAction)] -> FeatureX
runDMenuShowKeys kbs =
  featureDefault "keyboard shortcut menu" (Only $ exe myDmenuCmd) $ io $ do
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
  featureDefault "clipboard manager" (And (Only $ exe myDmenuCmd) (Only $ exe "greenclip"))
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
  featureExeArgs "network control menu" myDmenuNetworks $ themeArgs "#ff3333"

runAutorandrMenu :: FeatureX
runAutorandrMenu =
  featureExeArgs "autorandr menu" myDmenuMonitors $ themeArgs "#ff0066"
