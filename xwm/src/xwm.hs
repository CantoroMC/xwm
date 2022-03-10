import XMonad

import           XMonad.User.Bindings.Binder   ( mapBindings, storeBindings )
import           XMonad.User.Bindings.Keys     ( xwmKeys )
import           XMonad.User.Bindings.Mouse    ( xwmMouseBindings )
import           XMonad.User.Layout.Workspaces ( xwmWorkspaces )


main = do
    let (applicableKeys, explainableBindings) = mapBindings $ xwmKeys . modMask
        xwmConfig = def {
            terminal             = "kitty"
            , focusFollowsMouse  = False
            , clickJustFocuses   = True
            , borderWidth        = 1
            , modMask            = mod4Mask
            , workspaces         = xwmWorkspaces
            , normalBorderColor  = "#000000"
            , focusedBorderColor = "#79ff0f"
            , keys               = applicableKeys
            , mouseBindings      = xwmMouseBindings
            , manageHook         = myManageHook
            , handleEventHook    = myEventHook
            , logHook            = myLogHook
            , startupHook        = myStartupHook
            , layoutHook         = myLayout
        }
        xwm =
            storeBindings explainableBindings xwmConfig
    xmonad xwm

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
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
    , className =? "Gimp"           --> doFloat
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
myStartupHook = return ()
