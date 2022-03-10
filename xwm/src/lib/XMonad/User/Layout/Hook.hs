{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.User.Layout.Hook ( xwmLayoutHook ) where

import XMonad                              ( (|||), Full(Full), Mirror(Mirror) )
import XMonad.Layout.MultiToggle           ( mkToggle, single )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers(MIRROR, NBFULL) )
import XMonad.Hooks.ManageDocks            ( avoidStrutsOn )
import XMonad.Layout.PerWorkspace          ( onWorkspace )
import XMonad.Util.Types                   ( Direction2D(U) )

import XMonad.User.Layout.Layouts
    ( xwmTall
    , xwmThreeCol
    , xwmTwoPane
    , xwmFloat
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
    ||| xwmFloat

xwmFloatWSLayouts =
    xwmFloat
    ||| xwmTall

xwmWideWSLayouts =
    xwmThreeCol
    ||| xwmTall
    ||| xwmTwoPane
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
