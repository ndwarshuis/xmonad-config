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
import           System.Directory (getXdgDirectory, XdgDirectory(..))

import           XMonad.Core              hiding (spawn)
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
runDevMenu = runIfInstalled [myDmenuDevices] $ do
  c <- io $ getXdgDirectory XdgConfig "rofi/devices.yml"
  spawnCmd myDmenuDevices
    $ ["-c", c]
    ++ "--" : themeArgs "#999933"
    ++ myDmenuMatchingArgs

runBwMenu :: IO MaybeX
runBwMenu = runIfInstalled [myDmenuPasswords] $
  spawnCmd myDmenuPasswords $ ["-c", "--"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs

-- TODO what to do with this if rofi doesn't exist?
runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys x = addName "Show Keybindings" $ do
  (h, _, _, _) <- io $ createProcess' $ (shell' cmd) { std_in = CreatePipe }
  io $ forM_ h $ \h' -> hPutStr h' (unlines $ showKm x) >> hClose h'
  where cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
          ++ themeArgs "#a200ff" ++ myDmenuMatchingArgs

runCmdMenu :: IO MaybeX
runCmdMenu = spawnDmenuCmd ["-show", "run"]

runAppMenu :: IO MaybeX
runAppMenu = spawnDmenuCmd ["-show", "drun"]

-- TODO this also depends on greenclip
runClipMenu :: IO MaybeX
runClipMenu = spawnDmenuCmd $
  [ "-modi", "\"clipboard:greenclip print\""
  , "-show", "clipboard"
  , "-run-command", "'{cmd}'"
  ] ++ themeArgs "#00c44e"

runWinMenu :: IO MaybeX
runWinMenu = spawnDmenuCmd ["-show", "window"]

runNetMenu :: IO MaybeX
runNetMenu = spawnCmdIfInstalled myDmenuNetworks $ themeArgs "#ff3333"

runAutorandrMenu :: IO MaybeX
runAutorandrMenu = spawnCmdIfInstalled myDmenuMonitors $ themeArgs "#ff0066"
