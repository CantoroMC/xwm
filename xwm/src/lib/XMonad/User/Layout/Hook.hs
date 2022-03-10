{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.User.Layout.Hook ( xwmLayoutHook ) where

import XMonad                         ( (|||), Full(Full), Mirror(Mirror) )
import XMonad.Hooks.ManageDocks       ( avoidStrutsOn, AvoidStruts )

import XMonad.User.Layout.Layouts
    ( xwmTall
    , xwmThreeCol
    , xwmTwoPane
    )

xwmLayoutHook =
    xwmTall
    ||| Mirror xwmTall
    ||| Full
    ||| xwmThreeCol
    ||| Mirror xwmThreeCol
    ||| xwmTwoPane
