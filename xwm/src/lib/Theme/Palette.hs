module Theme.Palette ( Color, Colors(..), palette ) where

import XMonad           ( Dimension )
import Theme.XResources ( xprop )

type Color = String
data Colors = Colors
    { background0 :: Color
    , foreground0 :: Color
    , background1 :: Color
    , foreground1 :: Color
    , black       :: Color
    , red         :: Color
    , green       :: Color
    , yellow      :: Color
    , magenta     :: Color
    , blue        :: Color
    , cyan        :: Color
    , white       :: Color
    , bBlack      :: Color
    , bRed        :: Color
    , bGreen      :: Color
    , bYellow     :: Color
    , bBlue       :: Color
    , bMagenta    :: Color
    , bCyan       :: Color
    , bWhite      :: Color
    , borderWidth :: Dimension
    } deriving (Eq, Read, Show)

palette :: Colors
palette = Colors
    { background0 = xprop "*background"
    , foreground0 = xprop "*foreground"
    , background1 = xprop "*alt_background"
    , foreground1 = xprop "*alt_foreground"
    , black       = xprop "*.color0"
    , red         = xprop "*.color1"
    , green       = xprop "*.color2"
    , yellow      = xprop "*.color3"
    , blue        = xprop "*.color4"
    , magenta     = xprop "*.color5"
    , cyan        = xprop "*.color6"
    , white       = xprop "*.color7"
    , bBlack      = xprop "*.color8"
    , bRed        = xprop "*.color9"
    , bGreen      = xprop "*.color10"
    , bYellow     = xprop "*.color11"
    , bBlue       = xprop "*.color12"
    , bMagenta    = xprop "*.color13"
    , bCyan       = xprop "*.color14"
    , bWhite      = xprop "*.color15"
    , borderWidth = 1
    }
