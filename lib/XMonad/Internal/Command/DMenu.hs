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

--------------------------------------------------------------------------------
-- | Exported Commands

devSecrets :: [String]
devSecrets = concatMap (\x -> ["-s", x])
  [ "/media/ndwar/Roylab:user=ndwarshuis3@gatech.edu,host=outlook.office365.com"
  , "/media/ndwar/MC3M:user=ndwarshuis3@gatech.edu,host=outlook.office365.com"
  ]

myDmenuMatchingArgs :: [String]
myDmenuMatchingArgs = ["-i"] -- case insensitivity

runDevMenu :: X ()
runDevMenu = spawnCmd "rofi-dev" $ devSecrets ++ rofiArgs
  where
    rofiArgs =
      [ "--"
      , "-theme-str"
      , "'#element.selected.normal { background-color: #999933; }'"
      ] ++
      myDmenuMatchingArgs

runBwMenu :: X ()
runBwMenu = spawnCmd "rofi-bw" $
  [ "-c"
  , "--"
  , "-theme-str"
  , "'#element.selected.normal { background-color: #bb6600; }'"
  ] ++
  myDmenuMatchingArgs

runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys x = addName "Show Keybindings" $ do
  (h, _, _, _) <- io $ createProcess' $ (shell' cmd) { std_in = CreatePipe }
  io $ forM_ h $ \h' -> hPutStr h' (unlines $ showKm x) >> hClose h'
  where cmd = fmtCmd myDmenuCmd $
          [ "-dmenu"
          , "-p", "commands"
          , "-theme-str"
          , "'#element.selected.normal { background-color: #a200ff; }'"
          ] ++
          myDmenuMatchingArgs

runCmdMenu :: X ()
runCmdMenu = spawnDmenuCmd ["-show", "run"]

runAppMenu :: X ()
runAppMenu = spawnDmenuCmd ["-show", "drun"]

runClipMenu :: X ()
runClipMenu = spawnDmenuCmd
  [ "-modi", "\"clipboard:greenclip print\""
  , "-show", "clipboard"
  , "-run-command", "'{cmd}'"
  , "-theme-str", "'#element.selected.normal { background-color: #00c44e; }'"
  ]

runWinMenu :: X ()
runWinMenu = spawnDmenuCmd ["-show", "window"]

runNetMenu :: X ()
runNetMenu = spawnCmd "networkmanager_dmenu" []

