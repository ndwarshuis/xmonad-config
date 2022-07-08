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

import           DBus
import           DBus.Client

import           Graphics.X11.Types

import           System.Directory
    ( XdgDirectory (..)
    , getXdgDirectory
    )
import           System.IO

import           XMonad.Core                 hiding (spawn)
import           XMonad.Internal.DBus.Common
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

-- TODO test that veracrypt and friends are installed
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

-- TODO test that bluetooth interface exists
runBTMenu :: SometimesX
runBTMenu = sometimesExeArgs "bluetooth selector" "rofi bluetooth" False
  myDmenuBluetooth $ "-c":themeArgs "#0044bb"

runVPNMenu :: SometimesX
runVPNMenu = sometimesIO_ "VPN selector" "rofi VPN" tree $ spawnCmd myDmenuVPN
  $ ["-c"] ++ themeArgs "#007766" ++ myDmenuMatchingArgs
  where
    tree = toAnd_ (localExe myDmenuVPN) $ socketExists "expressVPN"
      $ return "/var/lib/expressvpn/expressvpnd.socket"

runCmdMenu :: SometimesX
runCmdMenu = spawnDmenuCmd "command menu" ["-show", "run"]

runAppMenu :: SometimesX
runAppMenu = spawnDmenuCmd "app launcher" ["-show", "drun"]

runWinMenu :: SometimesX
runWinMenu = spawnDmenuCmd "window switcher" ["-show", "window"]

runNetMenu :: Maybe Client -> SometimesX
runNetMenu cl =
  sometimesDBus cl "network control menu" "rofi NetworkManager" tree cmd
  where
    cmd _ = spawnCmd myDmenuNetworks $ themeArgs "#ff3333"
    tree = toAnd_ (DBusIO $ localExe myDmenuNetworks) $ Bus networkManagerBus

runAutorandrMenu :: SometimesX
runAutorandrMenu = sometimesExeArgs "autorandr menu" "rofi autorandr"
  True myDmenuMonitors $ themeArgs "#ff0066"

--------------------------------------------------------------------------------
-- | Password manager

runBwMenu :: Maybe Client -> SometimesX
runBwMenu cl = sometimesDBus cl "password manager" "rofi bitwarden" tree cmd
  where
    cmd _ = spawnCmd myDmenuPasswords
      $ ["-c"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs
    tree = toAnd_ (DBusIO $ localExe myDmenuPasswords)
      $ Bus $ busName_ "org.rofi.bitwarden"

--------------------------------------------------------------------------------
-- | Clipboard

runClipMenu :: SometimesX
runClipMenu = sometimesIO_ "clipboard manager" "rofi greenclip" tree act
  where
    act = spawnCmd myDmenuCmd args
    tree = listToAnds (process myClipboardManager)
      $ sysExe <$> [myDmenuCmd, myClipboardManager]
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
  }

showKeys :: [((KeyMask, KeySym), NamedAction)] -> X ()
showKeys kbs = io $ do
      (h, _, _, _) <- createProcess' $ (shell' cmd) { std_in = CreatePipe }
      forM_ h $ \h' -> hPutStr h' (unlines $ showKm kbs) >> hClose h'
  where
    cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
      ++ themeArgs "#7f66ff" ++ myDmenuMatchingArgs
