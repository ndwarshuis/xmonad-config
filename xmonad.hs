--------------------------------------------------------------------------------
-- | Example.hs
--
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').
module Main (main) where

--------------------------------------------------------------------------------
import System.Exit
import System.IO

-- import Graphics.XOSD

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
-- import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
-- import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile (ResizableTall(..))
-- import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
-- import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
main = do
  h <- spawnPipe "xmobar"
  xmonad
    $ ewmh
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ def { terminal = myTerm
          , modMask = myModMask
          , layoutHook = myLayouts
          , manageHook = myManageHook <+> manageDocks <+> manageHook def
          , handleEventHook = docksEventHook <+> handleEventHook def
          , startupHook = docksStartupHook <+> startupHook def
          , workspaces = myWorkspaces
          , logHook = myLoghook h
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

myWorkspaces = map show [1..10 :: Int] ++ ["VM"]

-- this isn't perfect for Virtualbox because the border seems to be
-- required for hover-focus the controls bar at the top
myLayouts = onWorkspace "VM" (lessBorders OnlyScreenFloat Full) $
            (avoidStruts $ layoutHook def)

-- TODO hack dynamicLogXinerama and sort the screen by its xrandr
-- position (Graphics.X11.Xrandr?)
myLoghook h = dynamicLogWithPP $
              def { ppOutput = hPutStrLn h
                  , ppCurrent = wrap "<" ">"
                  , ppVisible = wrap "[" "]"
                  , ppTitle = const ""
                  , ppSep = " | "
                  , ppSort = getSortByXineramaRule }

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
-- myXPConfig = def
--   { position          = Top
--   , alwaysHighlight   = True
--   , promptBorderWidth = 0
--   , font              = "xft:monospace:size=9"
--   }

myManageHook = composeOne
  -- virtualbox seems to do whatever the "VirtualBoxVM" class
  -- window does, and as such I guess we only need to match that
  [ -- className =? "VirtualBox Machine" -?> doShift "3"
  className =? "VirtualBoxVM" -?> doShift "VM" <+> doFloat
  -- , className =? "URxvt" -?> doFloat
  , className =? "mpv"    -?> doFloat
  , isDialog              -?> doCenterFloat
  ]

-- themes
myFont = "xft:DejaVu Sans:size=11:autohint=false"

-- base00  = "#657b83"
-- base01  = "#586e75"
-- base02  = "#073642"
base03  = "#002b36"
-- base0   = "#839496"
-- base1   = "#93a1a1"
-- base2   = "#eee8d5"
base3   = "#fdf6e3"
-- yellow  = "#b58900"
-- orange  = "#cb4b16"
red     = "#dc322f"
-- magenta = "#d33682"
-- violet  = "#6c71c4"
blue    = "#268bd2"
-- cyan    = "#2aa198"
-- green = "#859900"

-- gap         = 10
-- topbar      = 10
-- border      = 0
prompt      = 20
-- status = 20

active      = blue
-- activeWarn  = red
-- inactive    = base02
-- focusColor  = blue
-- unfocusColor = base02

myPromptTheme = def
    { font = myFont
    , bgColor = base03
    , fgColor = active
    , fgHLight = base03
    , bgHLight = active
    , borderColor = base03
    , promptBorderWidth = 0
    , height = prompt
    , position = Top
}

hotPromptTheme = myPromptTheme
    { bgColor = red
    , fgColor = base3
    , position = Top
    }

-- osd

-- getOffset :: X Int
-- getOffset = withWindowSet $
--   \W.StackSet { W.current = W.Screen { W.screenDetail = SD { screenRect = Rectangle {rect_x=x}}}} -> return $
--   fromIntegral x

-- displayOsd osd msg = do
--   xpos <- getOffset
--   io $ set osd [HOffset xpos]
--   io $ Graphics.XOSD.display osd 0 msg

-- showVolume :: XOSD -> X ()
-- showVolume osd = do
--   volume <- io $ fmap round $ getVolumeChannels ["default"]
--   muted <- io $ getMute
--   displayOsd osd $ Percent $ if muted then 0 else volume

-- keybindings
myModMask = mod4Mask

_myRofi = "rofi -m -4" -- show rofi always with the focused window
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
myNetSel = "networkmanager_dmenu -m -4"
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
  -- , ("M--", addName "remove master window" $ sendMessage (Ingesting (-1)))
  -- ] ++

  mkNamedSubmap c "Workspaces"
  -- NOTE this assumes that there are workspaces bound to numbers
  ([ (mods ++ show i, addName (msg ++ " " ++ show i) $ windows $ f w)
    | (w, i) <- zip (XMonad.workspaces c) [1..] :: [(String, Int)]
    , (mods, msg, f) <-
      [ ("M-", "switch to workspace", W.view)
      , ("M-S-", "move client to workspace", W.shift)]
  ] ++
  [ ("M-v", addName "switch to VM workspace" $ windows $ W.view "VM")
  ]) ++

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

  mkNamedSubmap c "Multimedia"
  [ ("<XF86AudioPlay>", addName "toggle play/pause" $ spawn "playerctl play-pause")
  , ("<XF86AudioPrev>", addName "previous track" $ spawn "playerctl previous")
  , ("<XF86AudioNext>", addName "next track" $ spawn "playerctl next")
  , ("<XF86AudioStop>", addName "stop" $ spawn "playerctl stop")
  , ("<XF86AudioLowerVolume>", addName "volume down" $ lowerVolume 2 >> return ())
  , ("<XF86AudioRaiseVolume>", addName "volume up" $ raiseVolume 2 >> return ())
  , ("<XF86AudioMute>", addName "volume mute" $ toggleMute >> return ())
  , ("M-C-b", addName "toggle bluetooth" $ spawn "togglebt")
  ] ++

  mkNamedSubmap c "System"
  [ ("M-.", addName "backlight up" $ spawn "adj_backlight up")
  , ("M-,", addName "backlight down" $ spawn "adj_backlight down")
  , ("M-M1-,", addName "backlight min" $ spawn "adj_backlight min")
  , ("M-M1-.", addName "backlight max" $ spawn "adj_backlight max")
  , ("M-<F2>", addName "restart xmonad" $ spawn "killall xmobar; xmonad --restart")
  , ("M-S-<F2>", addName "recompile xmonad" $ spawn "killall xmobar; xmonad --recompile && xmonad --restart")
  , ("M-<Home>", addName "quit xmonad" $
      confirmPrompt hotPromptTheme "Quit XMonad?" $
      io (exitWith ExitSuccess))
  ]
