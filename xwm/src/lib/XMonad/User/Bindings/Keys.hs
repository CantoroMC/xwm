module XMonad.User.Bindings.Keys ( xwmKeys ) where

import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import           System.Exit                          (exitSuccess)


import           XMonad
import qualified XMonad.StackSet as XMSS
import           XMonad.Util.Ungrab                   ( unGrab )


import           XMonad.User.Bindings.Binder
    ( Binder
    , (|/-)
    , (^>)
    , (...)
    , bind
    , bindZip
    , getBindings
    )
import           XMonad.User.Bindings.Util
    ( terminalFromConf
    , inTerminalFromConf
    )
import           XMonad.User.Layout.Workspaces                   ( xwmWorkspaces )

-------------------------------------------------------------------------------
    -- Keymap
xwmKeys :: KeyMask -> Binder ()
xwmKeys mask = do
    ---------------------------------------------------------------------------
        -- Left side characters
    bind $ mask ... xK_q
        |/- "Restart xwm"
        ^> restart "xwm" True
    bind $ mask .|. shiftMask ... xK_q
        |/- "Kill focused client"
        ^> kill
    bind $ mask ... xK_t
        |/- "Push focused client back into tiling"
        ^> withFocused $ windows . XMSS.sink
    bind $ mask ... xK_a
        |/- "Spawn the secondary terminal"
        ^> spawn "st"
    bind $ mask .|. shiftMask ... xK_a
        |/- "Spawn the default terminal"
        ^> spawn =<< terminalFromConf
    bind $ mask .|. controlMask ... xK_a
        |/- "Spawn a tabbed secondary terminal"
        ^> spawn "tabbed -c -r 2 st -w ''"
    bind $ mask ... xK_d
        |/- "Spawn tabbed zathura"
        ^> spawn "tabbed -c zathura -e"
    bind $ mask .|. shiftMask ... xK_d
        |/- "Spawn evince"
        ^> spawn "evince"
    bind $ mask ... xK_f
        |/- "Spawn web browser"
        ^> spawn "google-chrome-stable"
    bind $ mask .|. shiftMask ... xK_f
        |/- "Spawn secondary web browser"
        ^> spawn "surf-open"
    ---------------------------------------------------------------------------
        -- Right side characters
    bind $ mask ... xK_u
        |/- "Spawn dmenu launcher"
        ^> spawn "dmenu_run"
    bind $ mask .|. shiftMask ... xK_u
        |/- "Spawn rofi launcher"
        ^> spawn "rofi -modi drun,run,combi -show combi"
    bind $ mask .|. controlMask ... xK_u
        |/- "Spawn xmenu launcher"
        ^> spawn "xmenu-apps"
    bind $ mask ... xK_k
        |/- "Move focus to the previous window"
        ^> windows XMSS.focusUp
    bind $ mask ... xK_j
        |/- "Move focus to the next window"
        ^> windows XMSS.focusDown
    bind $ mask .|. shiftMask ... xK_k
        |/- "Swap the focused window with the previous window"
        ^> windows XMSS.swapUp
    bind $ mask .|. shiftMask ... xK_j
        |/- "Swap the focused window with the next window"
        ^> windows XMSS.swapDown
    bind $ mask ... xK_h
        |/- "Shrink the master area"
        ^> sendMessage Shrink
    bind $ mask ... xK_l
        |/- "Expand the master area"
        ^> sendMessage Expand
    bind $ mask .|. shiftMask ... xK_n
        |/- "Resize viewed windows to the correct size"
        ^> refresh
    bind $ mask ... xK_m
        |/- "Move focus to the master window"
        ^> windows XMSS.focusMaster
    ---------------------------------------------------------------------------
        -- Surrounding keys
    bind $ mask ... xK_Tab
        |/- "Cycle forward through the available layout algorithms"
        ^> sendMessage NextLayout
    bind $ mask .|. shiftMask ... xK_Tab
        |/-  "Reset the layouts on the current workspace to default"
        ^> setLayout =<< asks (layoutHook . config)
    bind $ mask ... xK_Delete
        |/- "Spawn shutdown menu"
        ^> spawn "xmenu-shutdown"
    bind $ mask .|. shiftMask ... xK_Delete
        |/- "Quit xmonad"
        ^> io exitSuccess
    bind $ mask .|. shiftMask ... xK_BackSpace
        |/- "Screenlocker"
        ^> spawn "loginctl lock-session"
    bind $ mask ... xK_Return
        |/- "Spawn the secondary terminal"
        ^> spawn "st"
    bind $ mask .|. shiftMask ... xK_Return
        |/- "Spawn the default terminal"
        ^> spawn =<< terminalFromConf
    bind $ mask .|. controlMask ... xK_Return
        |/- "Spawn a tabbed secondary terminal"
        ^> spawn "tabbed -c -r 2 st -w ''"
    bind $ mask ... xK_comma
        |/- "Increment the number of windows in the master area"
        ^> sendMessage $ IncMasterN 1
    bind $ mask ... xK_period
        |/- "Deincrement the number of windows in the master area"
        ^> sendMessage $ IncMasterN (-1)
    bind $ mask ... xK_Print
        |/- "Take fullscreen screenshot"
        ^> unGrab >> spawn "scrotwp -fd"
    bind $ mask .|. shiftMask ... xK_Print
        |/- "Select a rectangular region to take a screenshot"
        ^> unGrab >> spawn "scrotwp -sd"
    bind $ mask .|. controlMask ... xK_Print
        |/- "Take a screenshot of the current window"
        ^> unGrab >> spawn "scrotwp -wd"
    ---------------------------------------------------------------------------
        -- Arrow keys
    ---------------------------------------------------------------------------
        -- Numbers
    bindZip ((mask ...) <$> [ xK_1 .. xK_9 ])
        (("Switch to workspace " <>) . pure <$> [ '1' .. '9' ])
        (windows . XMSS.greedyView <$> xwmWorkspaces)
    bindZip ((mask .|. shiftMask ...) <$> [ xK_1 .. xK_9 ])
        (("Move focused client to workspace " <>) . pure <$> [ '1' .. '9' ])
        (windows . XMSS.shift <$> xwmWorkspaces)
    ---------------------------------------------------------------------------
        -- Fn and XF86 keys
    bind $ mask ... xK_F1
        |/- "Binding documentation"
        ^> do doc <- getBindings
              term <- terminalFromConf
              spawn $
                term
                <> " --override font_size=9 --name keysheet --title keysheet sh -c \"echo '"
                <> doc
                <> "' | less\""
    bind $ noModMask ... XF86.xF86XK_AudioMute
        |/- "Toggle mute/unmute audio"
        ^> spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    bind $ noModMask ... XF86.xF86XK_AudioLowerVolume
        |/- "Decrease volume"
        ^> spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
    bind $ noModMask ... XF86.xF86XK_AudioRaiseVolume
        |/- "Increase volume"
        ^> spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    bind $ noModMask ... XF86.xF86XK_MonBrightnessDown
        |/- "Decrease screen brightness"
        ^> spawn "xbacklight -dec 5"
    bind $ noModMask ... XF86.xF86XK_MonBrightnessUp
        |/- "Increase screen brightness"
        ^> spawn "xbacklight -inc 5"
    bind $ noModMask ... XF86.xF86XK_Display
        |/- "Configure monitor setup"
        ^> spawn "xrander"
    bind $ noModMask ... XF86.xF86XK_Search
        |/- "Spawn nnn"
        ^> spawn =<< inTerminalFromConf "nnn"
    bind $ noModMask ... XF86.xF86XK_Explorer
        |/- "Spawn web browser"
        ^> spawn "google-chrome-stable"
    bind $ noModMask ... XF86.xF86XK_Calculator
        |/- "Spawn calculator"
        ^> spawn =<< inTerminalFromConf "ghci"


--myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
--    --  Reset the layouts on the current workspace to default
--    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
--    -- Swap the focused window and the master window
--    , ((modm,               xK_Return), windows W.swapMaster)
--    -- Toggle the status bar gap
--    -- Use this binding with avoidStruts from Hooks.ManageDocks.
--    -- See also the statusBar function from Hooks.DynamicLog.
--    --
--    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
--    ]
--    ++
--    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
--    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--    --
--    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
