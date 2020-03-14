{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import ACPI
import SendXMsg

import Control.Monad (forM_, void, when)

import System.Exit
import System.IO

import Data.List (sortBy, sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (All(..))

import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types

import Text.Read (readMaybe)

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Volume
-- import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
-- import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout.BinarySpacePartition (emptyBSP)
-- import XMonad.Layout.DragPane
-- import XMonad.Layout.IM
-- import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
-- import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
-- import XMonad.Prompt.XMonad
-- import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run
-- import XMonad.Util.WindowProperties

import qualified XMonad.StackSet as W

main = do
  h <- spawnPipe "xmobar"
  spawn "powermon"
  xmonad
    $ ewmh
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ def { terminal = myTerm
          , modMask = myModMask
          , layoutHook = myLayouts
          , manageHook = myManageHook <+> manageDocks <+> manageHook def
          , handleEventHook = myEventHook <+> docksEventHook <+> handleEventHook def
          , startupHook = docksStartupHook <+> startupHook def
          , workspaces = myWorkspaces
          , logHook = myLoghook h
          , clickJustFocuses = False
          , focusFollowsMouse = False
          }

myTopBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = "#999999"
    , inactiveColor         = "#999999"
    , inactiveTextColor     = "#999999"
    , activeBorderColor     = "#d6d6d6"
    , activeColor           = "#d6d6d6"
    , activeTextColor       = "#d6d6d6"
    -- , urgentBorderColor     = red
    -- , urgentTextColor       = yellow
    , decoHeight            = 20
    }

myTabbedTheme = def
   { fontName = myFont
   , activeColor = "#d6d6d6"
   , activeTextColor = "black"
   , activeBorderColor = "#d6d6d6"
   , inactiveColor = "#999999"
   , inactiveTextColor = "#333333"
   , inactiveBorderColor = "#999999"
   }

myWorkspaces = map show [1..10 :: Int]

myVMWorkspace = "VM"
myGimpWorkspace = "GIMP"

myLayouts = onWorkspace myVMWorkspace (noBorders Full)
  -- $ onWorkspace myGimpWorkspace gimpLayout
  $ tall ||| single ||| full
  where
    addTopBar = noFrillsDeco shrinkText myTopBarTheme
    tall = named "Tall"
      $ avoidStruts
      $ addTopBar
      $ noBorders
      $ Tall 1 0.03 0.5
    single = named "Tabbed"
      -- $ addTopBar
      $ avoidStruts
      $ noBorders
      $ tabbedAlways shrinkText myTabbedTheme
    full = named "Full"
      $ noBorders Full
    -- gimpLayout = named "Gimp Layout"
    --   $ avoidStruts
    --   $ (tabbedAlways shrinkText defaultTheme) ****||* Full
    --   -- $ withIM (11/64) (Or (Title "Toolbox") (Title "Tool Options"))
    --   -- $ (tabbedAlways shrinkText defaultTheme)

-- | Format workspace and layout in loghook
-- The format will be like "[<1> 2 3] 4 5 | LAYOUT" where each digit
-- is the workspace and LAYOUT is the current layout. Each workspace
-- in the brackets is currently visible and the order reflects the
-- physical location of each screen. The "<>" is the workspace
-- that currently has focus
myLoghook h = withWindowSet $ io . hPutStrLn h . myWindowSetXinerama

myWindowSetXinerama ws = wsString ++ sep ++ layout
  where
    wsString = xmobarColor "#444444" "" $ onscreen ++ offscreen'
    offscreen' = if offscreen == "" then "" else " " ++ offscreen
    sep = xmobarColor "#888888" "" " : "
    onscreen = xmobarColor "#5574ad" visColor
      $ wrap " " " "
      $ unwords
      $ map (fmtTags . W.tag . W.workspace)
      . sortBy compareXCoord
      $ W.current ws : W.visible ws
    fmtTags t = if t == W.currentTag ws
      then xmobarColor "#2c2c2c" visColor t
      else t
    offscreen = unwords
      $ map W.tag
      . filter (isJust . W.stack)
      . sortOn W.tag
      $ W.hidden ws
    visColor = "#8fc7ff"
    layout = description . W.layout . W.workspace . W.current $ ws
    compareXCoord s0 s1 = compare x0 x1
      where
        (_, Rectangle x0 _ _ _) = getScreenIdAndRectangle s0
        (_, Rectangle x1 _ _ _) = getScreenIdAndRectangle s1

myManageHook = composeOne
  -- assume virtualbox is not run with the toolbar in fullscreen mode
  -- as this makes a new window that confusingly must go over the
  -- actual VM window
  [ className =? "VirtualBoxVM" -?> doShift "VM"
  -- the seafile applet
  , className =? "Seafile Client" -?> doFloat
  -- gnucash
  , (className =? "Gnucash" <&&> title =? "Transaction Import Assistant") -?> doFloat
  -- xsane
  , className =? "Xsane" -?> doFloat
  -- all of GIMP
  , className =? "Gimp" -?> doFloat
  -- , title =? "GIMP Startup" -?> doIgnore
  -- plots and graphics created by R
  , className =? "R_x11" -?> doFloat
  , className =? "mpv"    -?> doFloat
  -- the floating windows created by the brave browser
  , stringProperty "WM_NAME" =? "Brave" -?> doFloat
  , (stringProperty "WM_WINDOW_ROLE" =? "pop-up"
     <&&> className =? "Brave-browser") -?> doFloat
  -- the dialog windows created by the zotero addon in Google Docs
  , (className =? "Zotero" <&&> resource =? "Toplevel") -?> doFloat
  , isDialog              -?> doCenterFloat
  ]

-- This is a giant hack to "listen" for applications that close. Some
-- apps like Virtualbox go on their own workspace which is dynamically
-- created. But I want said workspace to disappear when the app
-- closes. This is actually hard. We can't just listen to
-- DestroyWindow events as VBox will "destroy" windows when it
-- switches to fullscreen and back. We also can't just monitor the
-- process from the window since WindowDestroy events don't have PIDs
-- attached to them. Therefore, the hack to make this all work is to
-- make a script fire when VirtualBox (and other apps that I want to
-- control in this manner) close. This script fires a bogus
-- ClientMessage event to the root window. This event will have a
-- BITMAP atom (which should do nothing) and a "magic string" in the
-- data field that can be intercepted here. When this event is
-- registered here, close the dynamic workspaces that are empty.
myEventHook ClientMessageEvent { ev_message_type = t, ev_data = d }
  | t == bITMAP = do
    let (magic, tag) = splitXMsg d
    if | magic == magicStringWS -> removeEmptyWorkspaceByTag' tag
       | magic == acpiMagic -> do
         let acpiTag = readMaybe tag :: Maybe ACPIEvent
         forM_ acpiTag $ \case
           Power -> myPowerPrompt
           Sleep -> confirmPrompt myPromptTheme "suspend?" runSuspend
           LidClose -> do
             status <- io isDischarging
             forM_ status $ \s -> runScreenLock >> when s runSuspend
       | otherwise -> return ()
    return (All True)
  | otherwise = return (All True)
myEventHook _ = return (All True)

removeEmptyWorkspaceByTag' tag = do
  -- TODO this function works by first hiding the workspace to be
  -- removed and then removing it. This won't work if there are no
  -- other hidden workspaces to take it's place. So, need to scan
  -- through the list of workspaces and swap the first one that is
  -- empty with the workspace to be removed. If it actually is empty,
  -- this will be enough to make it disappear.
  removeEmptyWorkspaceByTag tag

-- themes
myFont = "xft:DejaVu Sans:size=11:autohint=false"

-- base00  = "#657b83"
-- base01  = "#586e75"
-- base02  = "#073642"
-- base03  = "#002b36"
-- base0   = "#839496"
-- base1   = "#93a1a1"
-- base2   = "#eee8d5"
-- base3   = "#fdf6e3"
-- yellow  = "#b58900"
-- orange  = "#cb4b16"
-- red     = "#dc322f"
-- magenta = "#d33682"
-- violet  = "#6c71c4"
-- blue    = "#268bd2"
-- cyan    = "#2aa198"
-- green = "#859900"

-- gap         = 10
-- topbar      = 10
-- border      = 0
-- prompt      = 20
-- status = 20

-- active      = blue
-- activeWarn  = red
-- inactive    = base02
-- focusColor  = blue
-- unfocusColor = base02

myPromptTheme = def
    { font = myFont
    , bgColor = "#eeeeee"
    , fgColor = "#282828"
    , fgHLight = "white"
    , bgHLight = "#268bd2"
    , borderColor = "white"
    , promptBorderWidth = 0
    , height = 30
    , position = CenteredAt 0.5 0.5
}

-- hotPromptTheme = myPromptTheme
--     { bgColor = red
--     , fgColor = base3
--     , position = Top
--     }


-- TODO is there a better way to get the prompt to say what I want?
data PowerPrompt = PowerPrompt

instance XPrompt PowerPrompt where
    showXPrompt PowerPrompt = "Select Option: "

runScreenLock :: X ()
runScreenLock = spawn "screenlock"

runPowerOff :: X ()
runPowerOff = spawn "systemctl poweroff"

runSuspend :: X ()
runSuspend = spawn "systemctl suspend"

runHibernate :: X ()
runHibernate = spawn "systemctl hibernate"

runReboot :: X ()
runReboot = spawn "systemctl reboot"

myPowerPrompt :: X ()
myPowerPrompt = mkXPrompt PowerPrompt conf comps
  $ fromMaybe (return ())
  . (`lookup` commands)
  where
    comps = mkComplFunFromList' (map fst commands)
    conf = myPromptTheme
    commands =
      [ ("poweroff", runPowerOff)
      , ("suspend", runScreenLock >> runSuspend)
      , ("hibernate", runScreenLock >> runHibernate)
      , ("reboot", runReboot)
      ]

myQuitPrompt :: X ()
myQuitPrompt = confirmPrompt myPromptTheme "quit?" $ io exitSuccess

-- shell commands

formatCmd :: String -> [String] -> String
formatCmd cmd args = unwords $ cmd : args

spawnCmd :: String -> [String] -> X ()
spawnCmd cmd args = spawn $ formatCmd cmd args

(#!&&) :: String -> String -> String
cmdA #!&& cmdB = cmdA ++ " && " ++ cmdB

infixr 0 #!&&

magicStringWS :: String
magicStringWS = "%%%%%"

spawnCmdOwnWS :: String -> [String] -> String -> X ()
spawnCmdOwnWS cmd args ws = spawn
  $ formatCmd cmd args
  #!&& formatCmd "xit-event" [magicStringWS, ws]

spawnKill :: [String] -> X ()
spawnKill cmds = spawn $ formatCmd "killall" cmds

myTerm :: String
myTerm = "urxvt"

runTerm :: X ()
runTerm = spawn myTerm

runCalc :: X ()
runCalc = spawnCmd myTerm ["-e", "R"]

myDmenuCmd :: String
myDmenuCmd = "rofi"

-- TODO this almost works except when a workspace with no windows is
-- focuses. In this case, rofi considers the root window to be focused
-- and will showup wherever the mouse pointer is. Need a way to get
-- the focused workspace and translate that to a monitor number for
-- rofi to consume
myDmenuArgs :: [String]
myDmenuArgs = ["-m", "-4"] -- show rofi with the focused window

spawnDmenuCmd :: [String] -> X ()
spawnDmenuCmd args = spawnCmd myDmenuCmd $ myDmenuArgs ++ args

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
runNetMenu = spawnCmd "networkmanager_dmenu" myDmenuArgs

runDevMenu :: X ()
runDevMenu = spawn "rofi-devices"

runBrowser :: X ()
runBrowser = spawn "brave"

runEditor :: X ()
runEditor = spawnCmd "emacsclient"
  ["-c", "-e", "(select-frame-set-input-focus (selected-frame))\""]

runFileManager :: X ()
runFileManager = spawn "pcmanfm"

-- TODO this will steal focus from the current window (and puts it
-- in the root window?) ...need to fix
runScreenCap :: X ()
runScreenCap = spawn "flameshot gui"
-- myWindowCap = "screencap -w" --external script

runVBox :: X ()
runVBox = spawnCmdOwnWS "vbox-start win8raw" [] myVMWorkspace

runGimp :: X ()
runGimp = spawnCmdOwnWS "gimp" [] myGimpWorkspace

runCleanup :: X ()
runCleanup = spawnKill ["xmobar", "powermon"]

runRestart :: X ()
runRestart = spawnCmd "xmonad" ["--restart"]

runRecompile :: X ()
runRecompile = spawnCmd "xmonad" ["--recompile"]

myMultimediaCtl :: String
myMultimediaCtl = "playerctl"

runTogglePlay :: X ()
runTogglePlay = spawnCmd myMultimediaCtl ["play-pause"]

runPrevTrack :: X ()
runPrevTrack = spawnCmd myMultimediaCtl ["previous"]

runNextTrack :: X ()
runNextTrack = spawnCmd myMultimediaCtl ["next"]

runStopPlay :: X ()
runStopPlay = spawnCmd myMultimediaCtl ["stop"]

runVolumeDown :: X ()
runVolumeDown = void (lowerVolume 2)

runVolumeUp :: X ()
runVolumeUp = void (lowerVolume 2)

runVolumeMute :: X ()
runVolumeMute = void toggleMute

runToggleBluetooth :: X ()
runToggleBluetooth = spawn "togglebt"

runIncBacklight :: X ()
runIncBacklight = spawnCmd "adj_backlight" ["up"]

runDecBacklight :: X ()
runDecBacklight = spawnCmd "adj_backlight" ["down"]

runMinBacklight :: X ()
runMinBacklight = spawnCmd "adj_backlight" ["min"]

runMaxBacklight :: X ()
runMaxBacklight = spawnCmd "adj_backlight" ["max"]

showWorkspace tag = windows $ W.view tag

-- keybindings

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=DejaVu Sans"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

myModMask :: KeyMask
myModMask = mod4Mask

mkNamedSubmap
  :: XConfig l
     -> String
     -> [(String, NamedAction)]
     -> [((KeyMask, KeySym), NamedAction)]
mkNamedSubmap c sectionName bindings = 
  (subtitle sectionName:) $ mkNamedKeymap c bindings

-- NOTE: the following bindings are used by dunst:
-- "M-~", "M-<esc>", "M-S-<esc>", "M-S-."
myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  mkNamedSubmap c "Window Layouts"
  [ ("M-j", addName "focus down" $ windows W.focusDown)
  , ("M-k", addName "focus up" $ windows W.focusUp)
  , ("M-m", addName "focus master" $ windows W.focusMaster)
  , ("M-S-j", addName "swap down" $ windows W.swapDown)
  , ("M-S-k", addName "swap up" $ windows W.swapUp)
  , ("M-S-m", addName "swap master" $ windows W.swapMaster)
  -- TODO this will decrement past 0?
  , ("M-C-j", addName "remove master window" $ sendMessage (IncMasterN (-1)))
  , ("M-C-k", addName "add master window" $ sendMessage (IncMasterN 1))
  , ("M-<Return>", addName "next layout" $ sendMessage NextLayout)
  , ("M-S-<Return>", addName "reset layout" $ setLayout $ XMonad.layoutHook c)
  , ("M-t", addName "sink tiling" $ withFocused $ windows . W.sink)
  , ("M--", addName "shrink" $ sendMessage Shrink)
  , ("M-=", addName "expand" $ sendMessage Expand)
  ] ++

  mkNamedSubmap c "Workspaces"
  -- NOTE this assumes that there are workspaces bound to numbers
  ([ (mods ++ show i, addName (msg ++ " " ++ show i) $ windows $ f w)
    | (w, i) <- zip (XMonad.workspaces c) [1..] :: [(String, Int)]
    , (mods, msg, f) <-
      [ ("M-", "switch to workspace", W.view)
      , ("M-S-", "move client to workspace", W.shift)]
  ] ++
  [ ("M-v", addName "switch to VM workspace" $ showWorkspace myVMWorkspace)
  , ("M-M1-g", addName "switch to Gimp workspace" $ showWorkspace myGimpWorkspace)
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
  , ("M-r", addName "run program" runCmdMenu)
  , ("M-C-s", addName "capture screen area" runScreenCap)
  -- , ("M-C-S-s", addName "capture focused window" $ spawn myWindowCap)
  , ("M-<Delete>", addName "lock screen" runScreenLock)
  ] ++

  mkNamedSubmap c "Launchers"
  [ ("<XF86Search>", addName "select/launch app" runAppMenu)
  , ("M-g", addName "launch clipboard manager" runClipMenu)
  , ("M-a", addName "launch network selector" runNetMenu)
  , ("M-w", addName "launch window selector" runWinMenu)
  , ("M-u", addName "launch device selector" runDevMenu)
  , ("M-C-e", addName "launch editor" runEditor)
  , ("M-C-w", addName "launch browser" runBrowser)
  , ("M-C-t", addName "launch terminal" runTerm)
  , ("M-C-q", addName "launch calc" runCalc)
  , ("M-C-f", addName "launch file manager" runFileManager)
  , ("M-C-v", addName "launch windows VM" $ runVBox >> appendWorkspace myVMWorkspace)
  , ("M-C-g", addName "launch GIMP" $ runGimp >> appendWorkspace myGimpWorkspace)
  ] ++

  mkNamedSubmap c "Multimedia"
  [ ("<XF86AudioPlay>", addName "toggle play/pause" runTogglePlay)
  , ("<XF86AudioPrev>", addName "previous track" runPrevTrack)
  , ("<XF86AudioNext>", addName "next track" runNextTrack)
  , ("<XF86AudioStop>", addName "stop" runStopPlay)
  , ("<XF86AudioLowerVolume>", addName "volume down" runVolumeDown)
  , ("<XF86AudioRaiseVolume>", addName "volume up" runVolumeUp)
  , ("<XF86AudioMute>", addName "volume mute" runVolumeMute)
  , ("M-C-b", addName "toggle bluetooth" runToggleBluetooth)
  ] ++

  mkNamedSubmap c "System"
  [ ("M-.", addName "backlight up" runIncBacklight)
  , ("M-,", addName "backlight down" runDecBacklight)
  , ("M-M1-,", addName "backlight min" runMaxBacklight)
  , ("M-M1-.", addName "backlight max" runMinBacklight)
  , ("M-<F2>", addName "restart xmonad" $ runCleanup >> runRestart)
  , ("M-S-<F2>", addName "recompile xmonad" $ runCleanup >> runRecompile)
  , ("M-<End>", addName "power menu" myPowerPrompt)
  , ("M-<Home>", addName "quit xmonad" myQuitPrompt)
  ]
