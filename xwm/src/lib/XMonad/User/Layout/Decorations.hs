module XMonad.User.Layout.Decorations ( xwmDecorationTheme ) where

import           XMonad                    ( Default(def) )
import           XMonad.Layout.Decoration  ( Theme(..) )
import qualified Theme.Palette             as XwmTheme

xwmDecorationTheme :: Theme
xwmDecorationTheme = def
    { fontName            = "xft:Operator Mono Lig Nerd Font:pixelsize=9"
    , decoHeight          = 15
    , decoWidth           = 200
    , activeColor         = XwmTheme.background0 XwmTheme.palette
    , inactiveColor       = XwmTheme.background1 XwmTheme.palette
    , urgentColor         = XwmTheme.red         XwmTheme.palette
    , activeBorderColor   = XwmTheme.background1 XwmTheme.palette
    , inactiveBorderColor = XwmTheme.background0 XwmTheme.palette
    , urgentBorderColor   = XwmTheme.yellow      XwmTheme.palette
    , activeTextColor     = XwmTheme.green       XwmTheme.palette
    , inactiveTextColor   = XwmTheme.foreground0 XwmTheme.palette
    , urgentTextColor     = XwmTheme.yellow      XwmTheme.palette
    , activeBorderWidth   = XwmTheme.borderWidth XwmTheme.palette
    , inactiveBorderWidth = XwmTheme.borderWidth XwmTheme.palette
    , urgentBorderWidth   = XwmTheme.borderWidth XwmTheme.palette + 1
    }
