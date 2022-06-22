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

-- import           Control.Monad.Reader

import           Graphics.X11.Types

import           System.Directory           (XdgDirectory (..), getXdgDirectory)
-- import           System.IO

import           XMonad.Core                hiding (spawn)
import           XMonad.Internal.Dependency
-- import           XMonad.Internal.Notify
-- import           XMonad.Internal.Process
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

spawnDmenuCmd :: String -> [String] -> SometimesX
spawnDmenuCmd n = sometimesExeArgs n True myDmenuCmd

themeArgs :: String -> [String]
themeArgs hexColor =
  [ "-theme-str"
  , "'#element.selected.normal { background-color: " ++ hexColor ++ "; }'"
  ]

myDmenuMatchingArgs :: [String]
myDmenuMatchingArgs = ["-i"] -- case insensitivity

--------------------------------------------------------------------------------
-- | Exported Commands

runDevMenu :: SometimesX
runDevMenu = sometimesIO "device manager" (Only $ Executable False myDmenuDevices) $ do
  c <- io $ getXdgDirectory XdgConfig "rofi/devices.yml"
  spawnCmd myDmenuDevices
    $ ["-c", c]
    ++ "--" : themeArgs "#999933"
    ++ myDmenuMatchingArgs

runBTMenu :: SometimesX
runBTMenu = sometimesExeArgs "bluetooth selector" False myDmenuBluetooth
  $ "-c":themeArgs "#0044bb"

runBwMenu :: SometimesX
runBwMenu = sometimesIO "password manager" (Only $ Executable False myDmenuPasswords) $
  spawnCmd myDmenuPasswords $ ["-c"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs

runVPNMenu :: SometimesX
runVPNMenu = sometimesIO "VPN selector" (Only $ Executable False myDmenuVPN) $
  spawnCmd myDmenuVPN $ ["-c"] ++ themeArgs "#007766" ++ myDmenuMatchingArgs

-- runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
-- runShowKeys x = addName "Show Keybindings" $ do
--   s <- io $ evalFeature $ runDMenuShowKeys x
--   ifSatisfied s
--     $ spawnNotify
--     $ defNoteError { body = Just $ Text "could not display keymap" }

-- TODO not sure what to do with this yet
runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys _ = NamedAction (skip :: (X ()))
  -- addName "Show Keybindings" $ evalAlways $ runDMenuShowKeys x

-- runDMenuShowKeys :: [((KeyMask, KeySym), NamedAction)] -> AlwaysX
-- runDMenuShowKeys kbs =
--   Option (runDMenuShowKeys' kbs) (Always runNotifyShowKeys)

-- runNotifyShowKeys :: X ()
-- runNotifyShowKeys = spawnNotify
--   $ defNoteError { body = Just $ Text "could not display keymap" }

-- runDMenuShowKeys' :: [((KeyMask, KeySym), NamedAction)] -> Subfeature (X ()) Tree
-- runDMenuShowKeys' kbs = Subfeature
--   { sfName = "keyboard shortcut menu"
--   , sfTree = IOTree (Standalone act) deps
--   , sfLevel = Warn
--   }
--   where
--     deps = Only $ Executable True myDmenuCmd
--     act = io $ do
--       (h, _, _, _) <- createProcess' $ (shell' cmd) { std_in = CreatePipe }
--       forM_ h $ \h' -> hPutStr h' (unlines $ showKm kbs) >> hClose h'
--     cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
--       ++ themeArgs "#7f66ff" ++ myDmenuMatchingArgs

runCmdMenu :: SometimesX
runCmdMenu = spawnDmenuCmd "command menu" ["-show", "run"]

runAppMenu :: SometimesX
runAppMenu = spawnDmenuCmd "app launcher" ["-show", "drun"]

runClipMenu :: SometimesX
runClipMenu = sometimesIO "clipboard manager" deps act
  where
    act = spawnCmd myDmenuCmd args
    deps = toAnd (Executable True myDmenuCmd) (Executable True "greenclip")
    args = [ "-modi", "\"clipboard:greenclip print\""
           , "-show", "clipboard"
           , "-run-command", "'{cmd}'"
           ] ++ themeArgs "#00c44e"

runWinMenu :: SometimesX
runWinMenu = spawnDmenuCmd "window switcher" ["-show", "window"]

runNetMenu :: SometimesX
runNetMenu =
  sometimesExeArgs "network control menu" True myDmenuNetworks $ themeArgs "#ff3333"

runAutorandrMenu :: SometimesX
runAutorandrMenu =
  sometimesExeArgs "autorandr menu" True myDmenuMonitors $ themeArgs "#ff0066"
