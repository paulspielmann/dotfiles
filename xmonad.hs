--------------
-- IMPORTS ---
---------------

import XMonad
import Data.Monoid
import System.Exit
import System.IO (hPutStrLn)

import XMonad.Actions.DwmPromote -- Promote windows like in dwm
import XMonad.Actions.Minimize
import XMonad.Actions.RotSlaves -- Rotate the slave stack
import XMonad.Actions.NoBorders

-- Layouts
import XMonad.Layout.BoringWindows
import XMonad.Layout.Grid -- Grid layout
-- import XMonad.Layout.IndependentScreens -- Independent screens like in dwm
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect -- Change orientation of layout
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing -- Gaps
import XMonad.Layout.SubLayouts -- Make dynamic sublayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns -- Three col layout
import XMonad.Layout.ShowWName
import XMonad.Layout.WindowNavigation -- Navigate through windows like in i3
import XMonad.Layout.ToggleLayouts -- Navigate through windows like in i3


import XMonad.ManageHook -- Hooks executed on all new windows
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows -- Manage opacity
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Prompt
import XMonad.Prompt.OrgMode (orgPrompt) -- Adds interaction with org files

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers
import XMonad.Util.Minimize
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-----------------
--- VARIABLES ---
-----------------

myNormalBorderColor = "#333333"
myFocusedBorderColor = "#B877DB"

-- myFont :: String
-- myFont = "xft:Hack Nerd Font Mono:regular:size=11"

myBorderWidth = 1

myTerminal = "alacritty"
myBrowser = "firefox"
myLauncher = "dmenu_run -fn 'Hack Nerd Font Mono' -nb '#292d3e' -nf '#cccccc' -sb '#c792ea'"
-- myLauncher = "rofi -show drun"

myModMask = mod4Mask

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- myWorkspaces = ["\xf015", "\xf268", "\xe795", "\xf15c", "\xfb6e"]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

----------------
--- KEYBINDS ---
----------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_d     ), spawn myLauncher)

    -- , ((modm .|. shiftMask, xK_c    ), spawn "chromium")

    , ((modm,               xK_f    ), spawn "firefox")

    -- File manager
    , ((modm .|. shiftMask, xK_f    ), spawn "spacefm")

    , ((modm .|. shiftMask, xK_d    ), spawn "discord && beautifuldiscord --css /home/Paul/.config/BeautifulDiscord/mytheme.css")

    , ((modm,               xK_Print), spawn "emacs")

    , ((modm,               xK_s), spawn "steam")

    , ((modm .|. shiftMask, xK_s), spawn "cd ~/Pictures && scrot -s")

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)

    -- Toggle fullscreen
    , ((modm .|. controlMask, xK_space), sendMessage (Toggle "full"))

    -- Toggle borders (we shouldn't need this but SmartBorders
    -- doesn't remove borders when watching a fullscreen video)
    , ((modm,               xK_b ), withFocused toggleBorder)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current work>space to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Rotate the slave stack up
    , ((modm,               xK_Tab), rotSlavesUp)

    -- Rotate the slave stack down
    , ((modm .|. shiftMask, xK_Tab), rotSlavesDown)

    -- Move focus to right window
    , ((modm,               xK_l), sendMessage $ Go R)

    -- Move focus to left window
    , ((modm,               xK_h ), sendMessage $ Go L)

    -- Move focus to upper window
    , ((modm,               xK_j ), sendMessage $ Go U)

    -- Move focus to lower window
    , ((modm,               xK_k ), sendMessage $ Go D)

    -- Swap with right window
    , ((modm .|. shiftMask, xK_l), sendMessage $ Swap R)

    -- Swap with left window
    , ((modm .|. shiftMask, xK_h ), sendMessage $ Swap L)

      -- Swap with upper window
    , ((modm .|. shiftMask, xK_j   ), sendMessage $ Swap U)

    -- Swap with lower window
    , ((modm .|. shiftMask, xK_k ), sendMessage $ Swap D)

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), dwmpromote)

    -- Shrink the master area
    , ((modm,               xK_u     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_o     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm .|. shiftMask, xK_o ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_u), sendMessage (IncMasterN (-1)))

    --- Sublayouts bindings ---
    -- Pull window directionally
    , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup U)
    , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup D)
    , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)

    -- Merge and unmerge
    , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

    -- Change window through the group
    , ((modm .|. controlMask, xK_comma), onGroup W.focusUp')
    , ((modm .|. controlMask, xK_semicolon), onGroup W.focusDown')

    -- Minimize window
    , ((modm,               xK_x     ), withFocused minimizeWindow)

    -- Maximize last minimized window
    , ((modm,               xK_z     ), withLastMinimized maximizeWindowAndFocus)

    -- Increase gaps
    , ((modm,               xK_equal ), incScreenWindowSpacing 1)

    -- Decrease gaps
    , ((modm,               xK_minus), incScreenWindowSpacing (-1))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm .|. shiftMask, xK_b     ), sendMessage ToggleStruts)

    -- Org prompt TODO
    , ((modm .|. shiftMask, xK_t     ), orgPrompt def "TODO" "/home/paul/org/todo.org")

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_a     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{w,e}, Move client to screen 1 or 2
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

---------------
--- LAYOUTS ---
---------------

tall = renamed [XMonad.Layout.Renamed.Replace "tall"]
       $ spacingWithEdge 3
       $ avoidStruts
       $ minimize
       $ boringWindows
       $ windowNavigation
       $ subTabbed
       -- $ reflectHoriz
       $ Tall 1 (3/100) (55/100)

grid = renamed [XMonad.Layout.Renamed.Replace "grid"]
       $ spacingWithEdge 3
       $ avoidStruts
       $ minimize
       $ boringWindows
       $ windowNavigation
       $ subTabbed
       $ Grid

threecol = renamed [XMonad.Layout.Renamed.Replace "threecol"]
       $ spacingWithEdge 3
       $ avoidStruts
       $ minimize
       $ boringWindows
       $ windowNavigation
       $ subTabbed
       $ reflectHoriz
       $ ThreeColMid 1 (3/100) (55/100)

full = renamed [XMonad.Layout.Renamed.Replace "full"]
       $ noBorders
       $ minimize
       $ subTabbed
       $ Full

myLayout = toggleLayouts (noBorders full) (tall ||| grid ||| threecol)

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- myManageHook = composeAll
--   [ className =? "Steam" --> doFloat
--   , willFloat --> hasBorder False]

------------------------------------------------------------------------

myEventHook = fadeWindowsEventHook

myFadeHook = composeAll
  [ opacity 0.9
  , className =? "Steam" --> opaque
  , title =? "*.pdf" --> opaque
  , className =? "discord" --> opaque
  , className =? "xmobar" --> opaque]

------------------------------------------------------------------------
-- Log hooks

myLogHook = fadeWindowsLogHook myFadeHook

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
myPP = def { ppCurrent = xmobarFont 0 . xmobarColor "#c792ea" "" . wrap "[""]"
           , ppVisible = xmobarFont 0 . xmobarColor "#c792ea" ""
           , ppHidden = xmobarFont 0 . xmobarColor "white" "" . wrap "*" ""
           , ppHiddenNoWindows = xmobarFont 0 . xmobarColor "white" ""
           , ppLayout = xmobarColor "#89DDFF" "" . wrap "<" ">"
           }
mySBL = statusBarProp "dbus-launch xmobar --screen 0" $ (pure myPP)
mySBR = statusBarProp "dbus-launch xmobar --screen 1" $ (pure myPP)

mySort = getSortByXineramaRule

main = do
  xmonad . withSB (mySBL <> mySBR) . ewmhFullscreen . ewmh . docks $ def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        -- manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook
    }
