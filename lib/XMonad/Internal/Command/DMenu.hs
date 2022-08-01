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

import           Data.Internal.DBus
import           Data.Internal.Dependency

import           DBus

import           Graphics.X11.Types

import           System.Directory
    ( XdgDirectory (..)
    , getXdgDirectory
    )
import           System.IO

import           XMonad.Core                     hiding (spawn)
import           XMonad.Internal.Command.Desktop
import           XMonad.Internal.DBus.Common
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
-- | Packages

dmenuPkgs :: [Fulfillment]
dmenuPkgs = [Package Official "rofi"]

clipboardPkgs :: [Fulfillment]
clipboardPkgs = [Package AUR "rofi-greenclip"]

--------------------------------------------------------------------------------
-- | Other internal functions

spawnDmenuCmd :: String -> [String] -> SometimesX
spawnDmenuCmd n =
  sometimesExeArgs n "rofi preset" dmenuPkgs True myDmenuCmd

themeArgs :: String -> [String]
themeArgs hexColor =
  [ "-theme-str"
  , "'#element.selected.normal { background-color: " ++ hexColor ++ "; }'"
  ]

myDmenuMatchingArgs :: [String]
myDmenuMatchingArgs = ["-i"] -- case insensitivity

dmenuTree :: IOTree_ -> IOTree_
dmenuTree = And_ $ Only_ dmenuDep

dmenuDep :: IODependency_
dmenuDep = sysExe dmenuPkgs myDmenuCmd

--------------------------------------------------------------------------------
-- | Exported Commands

-- TODO test that veracrypt and friends are installed
runDevMenu :: SometimesX
runDevMenu = sometimesIO_ "device manager" "rofi devices" t x
  where
    t = dmenuTree $ Only_ (localExe [] myDmenuDevices)
    x = do
      c <- io $ getXdgDirectory XdgConfig "rofi/devices.yml"
      spawnCmd myDmenuDevices
        $ ["-c", c]
        ++ "--" : themeArgs "#999933"
        ++ myDmenuMatchingArgs

-- TODO test that bluetooth interface exists
runBTMenu :: SometimesX
runBTMenu = Sometimes "bluetooth selector" xpfBluetooth
  [Subfeature (IORoot_ cmd tree) "rofi bluetooth"]
  where
    cmd = spawnCmd myDmenuBluetooth $ "-c":themeArgs "#0044bb"
    tree = dmenuTree $ Only_ $ sysExe [] myDmenuBluetooth

runVPNMenu :: SometimesX
runVPNMenu = Sometimes "VPN selector" xpfVPN
  [Subfeature (IORoot_ cmd tree) "rofi VPN"]
  where
    cmd = spawnCmd myDmenuVPN
      $ ["-c"] ++ themeArgs "#007766" ++ myDmenuMatchingArgs
    tree = dmenuTree $ toAnd_ (localExe [] myDmenuVPN)
      $ socketExists "expressVPN" []
      $ return "/var/lib/expressvpn/expressvpnd.socket"

runCmdMenu :: SometimesX
runCmdMenu = spawnDmenuCmd "command menu" ["-show", "run"]

runAppMenu :: SometimesX
runAppMenu = spawnDmenuCmd "app launcher" ["-show", "drun"]

runWinMenu :: SometimesX
runWinMenu = spawnDmenuCmd "window switcher" ["-show", "window"]

runNetMenu :: Maybe SysClient -> SometimesX
runNetMenu cl = Sometimes "network control menu" enabled
  [Subfeature root "network control menu"]
  where
    enabled f = xpfEthernet f || xpfWireless f || xpfVPN f
    root = DBusRoot_ cmd tree cl
    cmd _ = spawnCmd myDmenuNetworks $ themeArgs "#ff3333"
    tree = And_ (Only_ $ Bus networkManagerPkgs networkManagerBus)
      $ toAnd_ (DBusIO dmenuDep) $ DBusIO
      $ sysExe [Package AUR "networkmanager-dmenu-git"] myDmenuNetworks

runAutorandrMenu :: SometimesX
runAutorandrMenu = sometimesIO_ "autorandr menu" "rofi autorandr" tree cmd
  where
    cmd = spawnCmd myDmenuMonitors $ themeArgs "#ff0066"
    tree = dmenuTree $ Only_ $ localExe [] myDmenuMonitors

--------------------------------------------------------------------------------
-- | Password manager

runBwMenu :: Maybe SesClient -> SometimesX
runBwMenu cl = sometimesDBus cl "password manager" "rofi bitwarden" tree cmd
  where
    cmd _ = spawnCmd myDmenuPasswords
      $ ["-c"] ++ themeArgs "#bb6600" ++ myDmenuMatchingArgs
    tree = And_ (Only_ $ Bus [] $ busName_ "org.rofi.bitwarden")
      $ toAnd_ (DBusIO dmenuDep) (DBusIO $ localExe [] myDmenuPasswords)

--------------------------------------------------------------------------------
-- | Clipboard

runClipMenu :: SometimesX
runClipMenu = sometimesIO_ "clipboard manager" "rofi greenclip" tree act
  where
    act = spawnCmd myDmenuCmd args
    tree = listToAnds dmenuDep [ sysExe clipboardPkgs myClipboardManager
                               , process [] myClipboardManager
                               ]
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
  , sfData = IORoot_ showKeys $ Only_ dmenuDep
  }

showKeys :: [((KeyMask, KeySym), NamedAction)] -> X ()
showKeys kbs = io $ do
      (h, _, _, _) <- createProcess' $ (shell' cmd) { std_in = CreatePipe }
      forM_ h $ \h' -> hPutStr h' (unlines $ showKm kbs) >> hClose h'
  where
    cmd = fmtCmd myDmenuCmd $ ["-dmenu", "-p", "commands"]
      ++ themeArgs "#7f66ff" ++ myDmenuMatchingArgs
