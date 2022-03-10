import XMonad
    ( XConfig
        ( terminal
        , focusFollowsMouse
        , clickJustFocuses
        , borderWidth
        , modMask
        , workspaces
        , normalBorderColor
        , focusedBorderColor
        , keys
        , mouseBindings
        , manageHook
        , startupHook
        , layoutHook
        )
    , mod4Mask
    , xmonad
    , Default(def) 
    )
import           XMonad.Hooks.EwmhDesktops     ( ewmh )
import           XMonad.Hooks.ManageDocks      ( docks )
import           XMonad.Hooks.Rescreen         ( rescreenHook )
import           XMonad.Hooks.StatusBar        ( withSB )

import qualified Theme.Palette                 as XwmTheme
import           XMonad.User.Bindings.Binder   ( mapBindings, storeBindings )
import           XMonad.User.Bindings.Keys     ( xwmKeys )
import           XMonad.User.Bindings.Mouse    ( xwmMouseBindings )
import           XMonad.User.Layout.Hook       ( xwmLayoutHook )
import           XMonad.User.Layout.Workspaces ( xwmWorkspaces )
import           XMonad.User.Log.StatusBar     ( xBarConfig )
import           XMonad.User.Manage.Hook       ( xwmManageHook )
import           XMonad.User.Manage.Util       ( applyUrgencyHook, xwmRescreenCfg )
import           XMonad.User.Startup.Hook      ( xwmStartupHook )

main :: IO ()
main = do
    let (applicableKeys, explainableBindings) = mapBindings $ xwmKeys . modMask
        xwmConfig = def {
            terminal             = "kitty"
            , focusFollowsMouse  = False
            , clickJustFocuses   = True
            , borderWidth        = XwmTheme.borderWidth XwmTheme.palette
            , modMask            = mod4Mask
            , workspaces         = xwmWorkspaces
            , normalBorderColor  = XwmTheme.background0 XwmTheme.palette
            , focusedBorderColor = XwmTheme.green       XwmTheme.palette
            , keys               = applicableKeys
            , mouseBindings      = xwmMouseBindings
            , manageHook         = xwmManageHook
            -- , handleEventHook    = xwmEventHook
            -- , logHook            = xwmLogHook xwmProc
            , startupHook        = xwmStartupHook
            , layoutHook         = xwmLayoutHook
        }
        xwm =
            storeBindings explainableBindings
            . docks
            . ewmh
            . applyUrgencyHook
            . rescreenHook xwmRescreenCfg
            . withSB xBarConfig $ xwmConfig
    xmonad xwm
