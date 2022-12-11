{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.User.Layout.Hook ( xwmLayoutHook ) where

import XMonad                              ( (|||), Full(Full), Mirror(Mirror) )
import XMonad.Hooks.ManageDocks            ( avoidStrutsOn )
import XMonad.Layout.MultiToggle           ( mkToggle, single )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(MIRROR, NBFULL) )
import XMonad.Util.Types                   ( Direction2D(U) )

import XMonad.User.Layout.Layouts
    ( xwmTall
    , xwmThreeCol
    , xwmGrid
    , xwmTwoPane
    , xwmFloat
    , xwmOneBig
    , applySpacing
    )
import XMonad.User.Layout.MultiToggle.TabBarDecoration ( XwmTabBar(XWMTABBAR) )

xwmLayoutHook =
    avoidStrutsOn [U]
    . applySpacing 0
    . mkToggle (single NBFULL)
    . mkToggle (single MIRROR)
    . mkToggle (single XWMTABBAR)
    $ xwmTall
        ||| xwmGrid
        ||| xwmThreeCol
        ||| xwmTwoPane
        ||| xwmOneBig
        ||| xwmFloat
