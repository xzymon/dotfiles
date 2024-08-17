--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Control.Monad.State
import Control.Monad.Trans.Maybe
import XMonad hiding (Screen)
import XMonad.Actions.CycleWS
import XMonad.Actions.LinkWorkspaces
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.IndependentScreens
import XMonad.StackSet
import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers (logLayoutOnScreen, logTitleOnScreen, shortenL, wrapL, xmobarColorL)
import Data.List
import Data.Map (fromList)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import Data.Maybe (fromJust)
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

grey1, grey2, grey3, grey4, cyan, orange :: String
grey1  = "#2B2E37"
grey2  = "#555E70"
grey3  = "#697180"
grey4  = "#8691A8"
cyan   = "#8BABF0"
orange = "#C45500"

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["0","1","2","3","4","5","6","7","8","9"]



actionPrefix, actionButton, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+"
actionButton = "` button="
actionSuffix = "</action>"

addActions :: [(String, Int)] -> String -> String
addActions [] ws = ws
addActions (x:xs) ws = addActions xs (actionPrefix ++ k ++ actionButton ++ show b ++ ">" ++ ws ++ actionSuffix)
    where k = fst x
          b = snd x

------------------------------------------------------------------------

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

workspaceOnCurrentScreen :: WSType
workspaceOnCurrentScreen = WSIs $ do
  s <- currentScreen
  return $ \x -> W.tag x /= "NSP" && isOnScreen s x


-- spiecie ekranow --
traverseScreens :: Applicative f =>
    (Screen   i l a sid sd -> f (Screen   i l a sid' sd')) ->
    (StackSet i l a sid sd -> f (StackSet i l a sid' sd'))
traverseScreens f (StackSet cur vis hid flt) = pure StackSet
    <*> f cur
    <*> traverse f vis
    <*> pure hid
    <*> pure flt

-- oof, what is currently named PhysicalWorkspace should really have
-- been named PhysicalWorkspaceId (and similarly for VirtualWorkspace)
type PhysicalWorkspace' = Workspace PhysicalWorkspace (Layout Window) Window
type PhysicalScreen = Screen PhysicalWorkspace (Layout Window) Window ScreenId ScreenDetail

swapVirtualWorkspace ::
    VirtualWorkspace ->
    PhysicalScreen ->
    MaybeT (State [PhysicalWorkspace']) PhysicalScreen
swapVirtualWorkspace vIdNew scr
    | vIdOld == vIdNew = return scr
    | otherwise = do
    (pre, here:post) <- gets (break pIdMatches)
    put (pre ++ [workspace scr] ++ post)
    return scr { workspace = here }
    where
    (sId, vIdOld) = unmarshall . tag . workspace $ scr
    pIdMatches ws = unmarshall (tag ws) == (sId, vIdNew)

viewOnAllScreens :: VirtualWorkspace -> WindowSet -> WindowSet
viewOnAllScreens vId ws = case mws' of
    Nothing -> ws
    Just ws' -> ws' { hidden = hidden' }
    where
    swapVirtualWorkspaces = traverseScreens (swapVirtualWorkspace vId) ws
    (mws', hidden') = runState (runMaybeT swapVirtualWorkspaces) (hidden ws)

-- /spiecie ekranow --

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask .|. controlMask , xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_d     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- launch audacity
    , ((modm .|. shiftMask .|. controlMask, xK_a     ), spawn "audacity")

    -- launch VS Code
    , ((modm .|. shiftMask .|. controlMask, xK_c     ), spawn "code")

    -- launch firefox
    , ((modm .|. shiftMask .|. controlMask, xK_f     ), spawn "firefox")

    -- launch GIMP
    , ((modm .|. shiftMask .|. controlMask, xK_g     ), spawn "gimp")

    -- launch chromium
    , ((modm .|. shiftMask .|. controlMask, xK_h     ), spawn "chromium")

    -- launch inkscape
    , ((modm .|. shiftMask .|. controlMask, xK_i     ), spawn "inkscape")

    -- launch idea
    , ((modm .|. shiftMask .|. controlMask, xK_j     ), spawn "x-idea")

    -- launch vlc
    , ((modm .|. shiftMask .|. controlMask, xK_v     ), spawn "vlc")

    -- sound mute
    , ((modm, xK_F3                                  ), spawn "x-mute")

    -- sound unmute
    , ((modm, xK_F4                                  ), spawn "x-unmute")

    -- sound -5%
    , ((modm, xK_F5                                  ), spawn "x-quieter")

    -- sound +5%
    , ((modm, xK_F6                                  ), spawn "x-louder")

    -- Print Screen
    , ((modm, xK_Print                               ), spawn "x-scrot")

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- screen controll
    , ((modm .|. controlMask, xK_o    ), switchScreen 0)
    , ((modm .|. controlMask, xK_Right), nextWS)
    , ((modm .|. controlMask, xK_Left ), prevWS)

    ,((modm .|. controlMask, xK_equal), toggleLinkWorkspaces defaultMessageConf)
    ,((modm .|. shiftMask .|. controlMask, xK_equal), removeAllMatchings defaultMessageConf)


    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask .|. controlMask, xK_e     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm .|. shiftMask .|. controlMask, xK_r     ), spawn "killall xmobar; xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]++

    --
    -- mod-[0..9], Switch to workspace N
    -- mod-shift-[0..9], Move client to workspace N && go to workspace N
    --
    -- wywolanie spietych ekranow
    -- to ponizej dodaje powiazanie pomiedzy workspaceami na roznych monitorach
    -- dzieki temu jak bedzie mod + nr to zostana wyswietlone spiete ekrany
    [((m .|. modm, k), windows f)
        | (i, k) <- zip (workspaces' conf) [xK_0 .. xK_9]
        , (f, m) <- [ (viewOnAllScreens i, 0), (viewOnAllScreens i . onCurrentScreen shift i, shiftMask)]
    ]++


    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
 ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> XMonad.focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> XMonad.focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> XMonad.focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------

-- | View next screen
--nextScreen :: X ()
--nextScreen = switchScreen 1

-- | View prev screen
--prevScreen :: X ()
--prevScreen = switchScreen (-1)

switchScreen :: Int -> X ()
switchScreen d = do s <- screenBy d
                    mws <- screenWorkspace s
                    warpToScreen s 0.618 0.618
                    case mws of
                         Nothing -> return ()
                         Just ws -> windows (W.view ws)



------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
--    , className =? "Gimp"           --> doFloat
--    , className =? "jetbrains-idea" --> doFloat
--    , className =? "code-oss"       --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawnOnce "wall-mgr &"
--  spawnOnce "xwallpaper --center /usr/share/sddm/themes/maldives/background.jpg &"
  spawnOnce "dunst &"
  spawnOnce "picom &"

------------------------------------------------------------------------

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [0..]

clickable :: [Char] -> [Char] -> [Char]
clickable icon ws = addActions [ (show i, 1), ("q", 2), ("Left", 4), ("Right", 5) ] icon
                    where i = fromJust $ M.lookup ws myWorkspaceIndices

myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
                    pure $ statusBarPropTo ("_XMONAD_LOG_" ++ show s)
                          ("xmobar -x " ++ show s ++ " ~/.config/xmonad/xmobar/xmobar" ++ show s ++ ".hs")
                          (pure $ myXmobarPP (S s))


myXmobarPP :: ScreenId -> PP
myXmobarPP s  = filterOutWsPP [scratchpadWorkspaceTag] . marshallPP s $ def
  { 
    ppSep = ""
  -- ^ separator to use between different log sections (window name, layout, workspaces)
  , ppWsSep = ""
  -- ^ separator to use between workspace tags
  , ppCurrent = xmobarColor cyan "" . clickable wsIconFull
  -- ^ how to print the tag of the currently focused workspace
  , ppVisible = xmobarColor grey4 "" . clickable wsIconFull
  -- ^ how to print tags of visible but not focused workspaces (xinerama only)
  , ppVisibleNoWindows = Just (xmobarColor grey4 "" . clickable wsIconFull)
  -- ^ how to print tags of empty visible workspaces
  , ppHidden = xmobarColor grey2 "" . clickable wsIconHidden
  -- ^ how to print tags of hidden workspaces which contain windows
  , ppHiddenNoWindows = xmobarColor grey2 "" . clickable wsIconEmpty
  -- ^ how to print tags of empty hidden workspaces
  , ppUrgent = xmobarColor orange "" . clickable wsIconFull
  -- ^ format to be applied to tags of urgent workspaces
  , ppOrder = \(ws : l : _ : extras) -> ws : l : extras
  -- ^ how to order the different log sections. By default, this function receives a list with three
  --   formatted strings, representing the workspaces, the layout, and the current window titles,
  --   respectively. If you have specified any extra loggers in 'ppExtras', their output will also be
  --   appended to the list.  To get them in the reverse order, you can just use @ppOrder = reverse@.
  --   If you don't want to display the current layout, you could use something like 
  --   @ppOrder = \\(ws:_:t:_) -> [ws,t]@, and so on.
  , ppExtras  = [ wrapL (actionPrefix ++ "n" ++ actionButton ++ "1>") actionSuffix
                $ wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix
                $ wrapL (actionPrefix ++ "Left" ++ actionButton ++ "4>") actionSuffix
                $ wrapL (actionPrefix ++ "Right" ++ actionButton ++ "5>") actionSuffix
                $ wrapL "    " "    " $ layoutColorIsActive s (logLayoutOnScreen s)
                , wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix
                $  titleColorIsActive s (shortenL 81 $ logTitleOnScreen s)
                ]
  }
  where
    wsIconFull   = "  <fn=2>\xf111</fn>   "
    wsIconHidden = "  <fn=2>\xf111</fn>   "
    wsIconEmpty  = "  <fn=2>\xf10c</fn>   "
    titleColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then xmobarColorL cyan "" l else xmobarColorL grey3 "" l
    layoutColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then wrapL "<icon=/home/xzymon/.config/xmobar/icons/" "_selected.xpm/>" l else wrapL "<icon=/home/xzymon/.config/xmobar/icons/" ".xpm/>" l

------------------------------------------------------------------------

-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
--  nScreens <- countScreens
--  xmproc <- spawnPipe "xmobar /home/xzymon/.config/xmobar/xmobarrc"
  xmonad
    . ewmh
    . ewmhFullscreen
    . dynamicSBs myStatusBarSpawner 
    . docks 
    $ defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        XMonad.workspaces  = withScreens 2 myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'super'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Ctrl-Shift-Enter  Launch alacritty (terminal)",
    "mod-Ctrl-Shift-a      Launch Audacity",
    "mod-Ctrl-Shift-c      Launch VS Code",
    "mod-Ctrl-Shift-f      Launch Firefox",
    "mod-Ctrl-Shift-g      Launch GIMP",
    "mod-Ctrl-Shift-h      Launch cHromium",
    "mod-Ctrl-Shift-i      Launch Inkscape",
    "mod-Ctrl-Shift-j      Launch intelliJ Idea",
    "mod-Ctrl-Shift-v      Launch VLC",
    "mod-F3                Sound - Mute",
    "mod-F4                Sound - Unmute",
    "mod-F5                Sound - Quieter",
    "mod-F6                Sound - Louder",
    "mod-PrintScreen       Screenshot (scrot)",
    "mod-p                 Launch dmenu",
    "mod-Shift-p           Launch gmrun",
    "mod-Shift-q           Close/kill the focused window",
    "mod-Space             Rotate through the available layout algorithms",
    "mod-Shift-Space       Reset the layouts on the current workSpace to default",
    "mod-n                 Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Ctrl-Shift-e   Quit xmonad",
    "mod-q              Restart xmonad",
    "mod-[0..9]         Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[0..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "mod-Ctrl-Left      Change workspace N to workspace N-1",
    "mod-Ctrl-Right     Change workspace N to workspace N+1",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging",
    "",
    "-- Running programs",
    "mod-button3  Set the window to floating mode and resize by dragging"]


-- | stare, sprzed moich zmian
-- | Finally, a copy of the default bindings in simple textual tabular format.
helpOld :: String
helpOld = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
