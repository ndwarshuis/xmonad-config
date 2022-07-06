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

  -- daemons
  , runBwDaemon
  , runClipManager
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

myClipboardManager :: String
myClipboardManager = "greenclip"

--------------------------------------------------------------------------------
-- | Other internal functions

spawnDmenuCmd :: String -> [String] -> SometimesX
spawnDmenuCmd n = sometimesExeArgs n "rofi preset" True myDmenuCmd

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
runDevMenu = sometimesIO_ "device manager" "rofi devices" t x
  where
    t = Only_ $ localExe myDmenuDevices
    x = do
      c <- io $ getXdgDirectory XdgConfig "rofi/devices.yml"
      spawnCmd myDmenuDevices
        $ ["-c", c]
        ++ "--" : themeArgs "#999933"
        ++ myDmenuMatchingArgs

runBTMenu :: SometimesX
runBTMenu = sometimesExeArgs "bluetooth selector" "rofi bluetooth" False
  myDmenuBluetooth $ "-c":themeArgs "#0044bb"


runVPNMenu :: SometimesX
runVPNMenu = sometimesIO_ "VPN selector" "rofi VPN"
  (Only_ $ localExe myDmenuVPN) $ spawnCmd myDmenuVPN
  $ ["-c"] ++ themeArgs "#007766" ++ myDmenuMatchingArgs

runCmdMenu :: SometimesX
runCmdMenu = spawnDmenuCmd "command menu" ["-show", "run"]

runAppMenu :: SometimesX
runAppMenu = spawnDmenuCmd "app launcher" ["-show", "drun"]

runWinMenu :: SometimesX
runWinMenu = spawnDmenuCmd "window switcher" ["-show", "window"]

runNetMenu :: SometimesX
runNetMenu = sometimesExeArgs "network control menu" "rofi NetworkManager"
  True myDmenuNetworks $ themeArgs "#ff3333"

runAutorandrMenu :: SometimesX
runAutorandrMenu = sometimesExeArgs "autorandr menu" "rofi autorandr"
  True myDmenuMonitors $ themeArgs "#ff0066"

--------------------------------------------------------------------------------
-- | Password manager

runBwDaemon :: Sometimes (IO ProcessHandle)
runBwDaemon = sometimesIO_ "password manager daemon" "rofi bitwarden" tree cmd
  where
    tree = Only_ $ localExe myDmenuPasswords
    cmd = snd <$> spawnPipeArgs "rofi-bw" ["-d", "3600"]

runBwMenu :: SometimesX
runBwMenu = sometimesIO_ "password manager" "rofi bitwarden"
  (Only_ $ IOSometimes_ runBwDaemon) $ spawnCmd myDmenuPasswords
  $ ["-c"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs

--------------------------------------------------------------------------------
-- | Clipboard

runClipManager :: Sometimes (IO ProcessHandle)
runClipManager = sometimesIO_ "clipboard daemon" "greenclip" tree cmd
  where
    tree = Only_ $ sysExe myClipboardManager
    cmd = snd <$> spawnPipeArgs "greenclip" ["daemon"]

runClipMenu :: SometimesX
runClipMenu = sometimesIO_ "clipboard manager" "rofi greenclip" tree act
  where
    act = spawnCmd myDmenuCmd args
    tree = toAnd_ (sysExe myDmenuCmd) $ IOSometimes_ runClipManager
    args = [ "-modi", "\"clipboard:greenclip print\""
           , "-show", "clipboard"
           , "-run-command", "'{cmd}'"
           ] ++ themeArgs "#00c44e"

--------------------------------------------------------------------------------
-- | Shortcut menu

runShowKeys :: Always ([((KeyMask, KeySym), NamedAction)] -> X ())
runShowKeys = Always "keyboard menu" $ Option showKeysDMenu $ Always_
  $ FallbackAlone fallback
  where
    -- TODO this should technically depend on dunst
    fallback = const $ spawnNotify
      $ defNoteError { body = Just $ Text "could not display keymap" }

showKeysDMenu :: SubfeatureRoot ([((KeyMask, KeySym), NamedAction)] -> X ())
showKeysDMenu = Subfeature
  { sfName = "keyboard shortcut menu"
  , sfData = IORoot_ showKeys $ Only_ $ sysExe myDmenuCmd
  , sfLevel = Warn
  }

showKeys :: [((KeyMask, KeySym), NamedAction)] -> X ()
showKeys kbs = io $ do
      (h, _, _, _) <- createProcess' $ (shell' cmd) { std_in = CreatePipe }
      forM_ h $ \h' -> hPutStr h' (unlines $ showKm kbs) >> hClose h'
  where
    cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
      ++ themeArgs "#7f66ff" ++ myDmenuMatchingArgs
