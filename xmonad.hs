--------------------------------------------------------------------------------
-- | Example.hs
--
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').
module Main (main) where

--------------------------------------------------------------------------------
-- import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
-- import XMonad.Config.Desktop
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
-- import XMonad.Layout.IndependentScreens
-- import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout.BinarySpacePartition (emptyBSP)
-- import XMonad.Layout.NoBorders (noBorders)
-- import XMonad.Layout.ResizableTile (ResizableTall(..))
-- import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
-- import XMonad.Prompt
-- import XMonad.Prompt.ConfirmPrompt
-- import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run

import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
main = do
  -- spawn "xmobar" -- Start a task bar such as xmobar.

  xmonad
    $ ewmh
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ def { terminal = myTerm
          , modMask = myModMask
          -- , workspaces = withScreens 2 ["name1", "name2"]
          -- , layoutHook = myLayoutHook
          }

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  -- xmonad $ desktopConfig
  --   { modMask    = mod4Mask -- Use the "Win" key for the mod key
  --   , manageHook = myManageHook <+> manageHook desktopConfig
  --   , layoutHook = desktopLayoutModifiers $ myLayouts
  --   , logHook    = dynamicLogString def >>= xmonadPropLog
  --   }

    -- `additionalKeysP` -- Add some extra key bindings:
    --   [ ("M-S-q",   confirmPrompt myXPConfig "exit" (io exitSuccess))
    --   , ("M-p",     shellPrompt myXPConfig)
    --   , ("M-<Esc>", sendMessage (Toggle "Full"))
    --   ]

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
-- myLayouts = toggleLayouts (noBorders Full) others
--   where
--     others = ResizableTall 1 (1.5/100) (3/5) [] ||| emptyBSP

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
-- myXPConfig = def
--   { position          = Top
--   , alwaysHighlight   = True
--   , promptBorderWidth = 0
--   , font              = "xft:monospace:size=9"
--   }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
-- myManageHook = composeOne
--   [ className =? "Pidgin" -?> doFloat
--   , className =? "XCalc"  -?> doFloat
--   , className =? "mpv"    -?> doFloat
--   , isDialog              -?> doCenterFloat

--     -- Move transient windows to their parent:
--   , transience
--   ]


-- keybindings
myModMask = mod4Mask

_myRofi = "rofi -m -2" -- show rofi always with the focused window
myTerm = "urxvt"
myBrowser = "brave"
myVBox = "VBoxManage startvm win8raw"
myEditor = "emacsclient -c -e \"(select-frame-set-input-focus (selected-frame))\""
myCalc = "urxvt -e R"
myRun = _myRofi ++ " -show run"
myAppRun = _myRofi ++ " -show drun"
myClipboard = _myRofi ++ " -modi \"clipboard:greenclip print\" \
              \-show clipboard -run-command '{cmd}' \
              \-theme-str '#element.selected.normal \
              \{ background-color: #00c44e; }'"
myNetSel = "networkmanager_dmenu -m -2"
myWinSel = _myRofi ++ " -show window"
myScreenCap = "screencap -s" --external script
myWindowCap = "screencap -w" --external script
myScreenLock = "screenlock" --external script

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=DejaVu Sans"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

mkNamedSubmap c sectionName bindings = 
  (subtitle sectionName:) $ mkNamedKeymap c bindings

myKeys c =
  mkNamedSubmap c "Window Focus"
  [ ("M-j", addName "focus down" $ windows W.focusDown)
  , ("M-k", addName "focus up" $ windows W.focusUp)
  , ("M-m", addName "focus master" $ windows W.focusMaster)
  ] ++

  mkNamedSubmap c "Window Layouts"
  [ ("M-S-j", addName "swap down" $ windows W.swapDown)
  , ("M-S-k", addName "swap up" $ windows W.swapUp)
  , ("M-<Return>", addName "swap master" $ windows W.swapMaster)
  , ("M-<Space>", addName "next layout" $ sendMessage NextLayout)
  , ("M-S-<Space>", addName "reset layout" $ setLayout $ XMonad.layoutHook c)
  , ("M-t", addName "sink tiling" $ withFocused $ windows . W.sink)
  ] ++

  mkNamedSubmap c "Window Sizing"
  [ ("M--", addName "shrink" $ sendMessage Shrink)
  , ("M-=", addName "expand" $ sendMessage Expand)
  ] ++

  -- mkNamedSubmap c "Master Windows"
  -- [ ("M-=", addName "add master window" $ sendMessage (IncMasterN 1))
  -- , ("M--", addName "remove master window" $ sendMessage (IncMasterN (-1)))
  -- ] ++

  mkNamedSubmap c "Workspaces"
  [ (mods ++ show i, addName (msg ++ " " ++ show i) $ windows $ f w)
    | (w, i) <- zip (XMonad.workspaces c) [1..] :: [(String, Int)]
    , (mods, msg, f) <-
      [ ("M-", "switch to workspace", W.view)
      , ("M-S-", "move client to workspace", W.shift)]
  ] ++

  mkNamedSubmap c "Screens"
  [ ("M-l", addName "move up screen" nextScreen)
  , ("M-h", addName "move down screen" prevScreen)
  , ("M-C-l", addName "move client up screen" $ shiftNextScreen >> nextScreen)
  , ("M-C-h", addName "move client down screen" $ shiftPrevScreen >> prevScreen)
  , ("M-S-l", addName "shift up screen" $ swapNextScreen >> nextScreen)
  , ("M-S-h", addName "shift down screen" $ swapPrevScreen >> prevScreen)
  ] ++

  mkNamedSubmap c "Actions"
  [ ("M-q", addName "close window" kill1)
  , ("M-r", addName "run program" $ spawn myRun)
  , ("M-C-s", addName "capture screen" $ spawn myScreenCap)
  , ("M-C-S-s", addName "capture focused window" $ spawn myWindowCap)
  , ("M-<Delete>", addName "lock screen" $ spawn myScreenLock)
  ] ++

  mkNamedSubmap c "Launchers"
  [ ("<XF86Search>", addName "select/launch app" $ spawn myAppRun )
  , ("M-g", addName "launch clipboard manager" $ spawn myClipboard )
  , ("M-a", addName "launch network selector" $ spawn myNetSel )
  , ("M-w", addName "launch window selector" $ spawn myWinSel )
  , ("M-C-e", addName "run editor" $ spawn myEditor)
  , ("M-C-w", addName "run browser" $ spawn myBrowser)
  , ("M-C-t", addName "run terminal" $ spawn myTerm)
  , ("M-C-q", addName "run calc" $ spawn myCalc)
  , ("M-C-v", addName "run windows VM" $ spawn myVBox)
  ] ++

  mkNamedSubmap c "System"
  [ ("M-.", addName "backlight up" $ spawn "adj_backlight up")
  , ("M-,", addName "backlight down" $ spawn "adj_backlight down")
  , ("M-<F2>", addName "restart xmonad" $ spawn "xmonad --restart")
  , ("M-S-<F2>", addName "recompile xmonad" $ spawn "xmonad --recompile && xmonad --restart")
  -- , ("M-<Home>", addName "quit xmonad" $
  --     confirmPrompt hotPromptTheme "Quit XMonad?" $
  --     io (exitWith ExitSuccess))
  ]
