{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.User.Layout.Hook ( xwmLayoutHook ) where

import XMonad                              ( (|||), Full(Full), Mirror(Mirror) )
import XMonad.Hooks.ManageDocks            ( avoidStrutsOn )
import XMonad.Layout.MultiToggle           ( mkToggle, single )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(MIRROR, NBFULL) )
import XMonad.Layout.PerWorkspace          ( onWorkspace )
import XMonad.Util.Types                   ( Direction2D(U) )

import XMonad.User.Layout.Layouts
    ( xwmTall
    , xwmThreeCol
    , xwmTwoPane
    , xwmFloat
    , xwmOneBig
    , applySpacing
    )
import XMonad.User.Layout.MultiToggle.TabBarDecoration ( XwmTabBar(XWMTABBAR) )
import XMonad.User.Layout.Workspaces                   ( xwmWorkspaces )

-------------------------------------------------------------------------------
    -- Per Workspace Combinations
xwmBaseWSLayouts =
    xwmTall
    ||| xwmThreeCol
    ||| xwmTwoPane
    ||| xwmOneBig
    ||| xwmFloat

xwmFloatWSLayouts =
    xwmFloat
    ||| xwmTall
    ||| xwmOneBig

xwmWideWSLayouts =
    xwmThreeCol
    ||| xwmTall
    ||| xwmTwoPane
    ||| xwmOneBig
    ||| xwmFloat

xwmLayouts =
    onWorkspace       (xwmWorkspaces !! 2) xwmWideWSLayouts
        $ onWorkspace (last xwmWorkspaces) xwmFloatWSLayouts
        xwmBaseWSLayouts

-------------------------------------------------------------------------------
    -- Layout Hook
xwmLayoutHook =
    avoidStrutsOn [U]
    . applySpacing 0
    . mkToggle (single NBFULL)
    . mkToggle (single MIRROR)
    . mkToggle (single XWMTABBAR)
    $ xwmLayouts
