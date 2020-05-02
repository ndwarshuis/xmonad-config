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

import           Data.List
import           Data.Maybe

import           Graphics.X11.Types
import           Graphics.X11.Xlib
import           Graphics.X11.Xrandr

import           System.IO

import           XMonad.Core              hiding (spawn)
import           XMonad.Internal.Process
import           XMonad.Internal.Shell
import           XMonad.StackSet
import           XMonad.Util.NamedActions

--------------------------------------------------------------------------------
-- | Fix rofi screen indexing limitations
--
-- Apparently xrandr and xinerama order monitors differently, which means they
-- have different indices. Since rofi uses the former and xmonad uses the
-- latter, these functions is to figure out the xrandr screen name based on the
-- xinerama screen that is currently in focus. The steps to do this:
-- 1) get the coordinates of the currently focuses xinerama screen
-- 2) get list of Xrandr outputs and filter which ones are connected
-- 3) match the coordinates of the xinerama screen with the xrandr output and
--    return the latter's name (eg "DP-0") which can be fed to Rofi

getMonitorName :: X (Maybe String)
getMonitorName = do
  dpy <- asks display
  root <- asks theRoot
  -- these are the focused screen coordinates according to xinerama
  (sx, sy) <- getCoords
  io $ do
    res <- xrrGetScreenResourcesCurrent dpy root
    outputs <- forM res $ \res' ->
      forM (xrr_sr_outputs res') $ \output -> do
        oi <- xrrGetOutputInfo dpy res' output
        case oi of
          -- connection: 0 == connected, 1 == disconnected
          Just XRROutputInfo { xrr_oi_connection = 0
                             , xrr_oi_name = name
                             , xrr_oi_crtc = crtc
                             } -> do
            ci <- xrrGetCrtcInfo dpy res' crtc
            return $ (\ci' -> Just (name, xrr_ci_x ci', xrr_ci_y ci'))
              =<< ci
          _ -> return Nothing
    return $ (\(name, _, _) -> Just name)
      =<< find (\(_, x, y) -> x == sx && y == sy) . catMaybes
      =<< outputs
  where
    getCoords = do
      (Rectangle x y _ _) <- getFocusedScreen
      return (fromIntegral x, fromIntegral y)

getFocusedScreen :: X Rectangle
getFocusedScreen = withWindowSet $ return . screenRect . screenDetail . current

--------------------------------------------------------------------------------
-- | Other internal functions

myDmenuCmd :: String
myDmenuCmd = "rofi"

spawnDmenuCmd :: String -> [String] -> X ()
spawnDmenuCmd cmd args = do
  name <- getMonitorName
  case name of
    Just n  -> spawnCmd cmd $ args ++ ["-m", n]
    Nothing -> io $ putStrLn "fail"

spawnDmenuCmd' :: [String] -> X ()
spawnDmenuCmd' = spawnDmenuCmd myDmenuCmd

--------------------------------------------------------------------------------
-- | Exported Commands

devSecrets :: [String]
devSecrets = concatMap (\x -> ["-s", x])
  [ "/media/ndwar/Roylab:user=ndwarshuis3@gatech.edu,host=outlook.office365.com"
  , "/media/ndwar/MC3M:user=ndwarshuis3@gatech.edu,host=outlook.office365.com"
  ]

runDevMenu :: X ()
runDevMenu = spawnDmenuCmd "rofi-dev" $ devSecrets ++ rofiArgs
  where
    rofiArgs =
      [ "--"
      , "-theme-str"
      , "'#element.selected.normal { background-color: #999933; }'"
      ]

runBwMenu :: X ()
runBwMenu = spawnDmenuCmd "rofi-bw"
  ["-c"
  , "--"
  , "-theme-str"
  , "'#element.selected.normal { background-color: #bb6600; }'"
  ]

runShowKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
runShowKeys x = addName "Show Keybindings" $ do
  name <- getMonitorName
  case name of
    Just n -> do
      (h, _, _, _) <- io $ createProcess' $ (shell' $ cmd n)
        { std_in = CreatePipe }
      io $ forM_ h $ \h' -> hPutStr h' (unlines $ showKm x) >> hClose h'
    Nothing -> io $ putStrLn "fail"
  where cmd name = fmtCmd myDmenuCmd
          [ "-dmenu"
          , "-m", name
          , "-p", "commands"
          , "-theme-str"
          , "'#element.selected.normal { background-color: #a200ff; }'"
          ]

runCmdMenu :: X ()
runCmdMenu = spawnDmenuCmd' ["-show", "run"]

runAppMenu :: X ()
runAppMenu = spawnDmenuCmd' ["-show", "drun"]

runClipMenu :: X ()
runClipMenu = spawnDmenuCmd'
  [ "-modi", "\"clipboard:greenclip print\""
  , "-show", "clipboard"
  , "-run-command", "'{cmd}'"
  , "-theme-str", "'#element.selected.normal { background-color: #00c44e; }'"
  ]

runWinMenu :: X ()
runWinMenu = spawnDmenuCmd' ["-show", "window"]

runNetMenu :: X ()
runNetMenu = spawnDmenuCmd "networkmanager_dmenu" []

