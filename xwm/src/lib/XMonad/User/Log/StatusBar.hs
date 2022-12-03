module XMonad.User.Log.StatusBar ( xBarConfig ) where

import           XMonad
    ( XState(windowset)
    , X
    , Default(def)
    , gets
    )
import qualified XMonad.StackSet              as XMSS
import           XMonad.Hooks.StatusBar       ( statusBarProp, StatusBarConfig )
import           XMonad.Hooks.StatusBar.PP
    ( PP( ppCurrent
        , ppVisible
        , ppHidden
        , ppHiddenNoWindows
        , ppVisibleNoWindows
        , ppUrgent
        , ppRename
        , ppSep
        , ppWsSep
        , ppTitle
        , ppTitleSanitize
        , ppLayout
        , ppOrder
        , ppSort
        , ppExtras
        , ppOutput
        )
    , pad
    , shorten
    , wrap
    , xmobarStrip
    , filterOutWsPP
    )
import           XMonad.Util.NamedScratchpad  ( scratchpadWorkspaceTag )
import           XMonad.Util.WorkspaceCompare ( getSortByIndex )

import qualified Theme.Palette                as XwmTheme
import XMonad.User.Log.ClickableWorkspaces    ( clickablePP )
import Xmobar.User.Util                       ( fn, fc, action, border, sep )


windowCount :: X (Maybe String)
windowCount =
    gets
        $ Just
        . show
        . length
        . XMSS.integrate'
        . XMSS.stack
        . XMSS.workspace
        . XMSS.current
        . windowset

xBarPP :: PP
xBarPP = def
    { ppCurrent =
        fc (XwmTheme.green XwmTheme.palette) ""
        . border "Top" (XwmTheme.green XwmTheme.palette) 2
        . pad
        . fn 3
    , ppVisible =
        fc (XwmTheme.foreground0 XwmTheme.palette) ""
        . border "Bottom" (XwmTheme.foreground0 XwmTheme.palette) 1
        . pad
        . fn 3
    , ppHidden =
        pad
        . border "Bottom" (XwmTheme.blue XwmTheme.palette) 2
        . fn 3
    , ppHiddenNoWindows  =
        fc (XwmTheme.background1 XwmTheme.palette) ""
        . pad
        . fn 3
    , ppVisibleNoWindows = Nothing
    , ppUrgent =
        wrap "(" ")"
        . fc (XwmTheme.red XwmTheme.palette) ""
        . pad
        . fn 3
    , ppRename           = pure
    , ppSep              = sep
    , ppWsSep            = ""
    , ppTitle            = fn 2 . shorten 40
    , ppTitleSanitize    = xmobarStrip
    , ppLayout           =
        action "xdotool key 0xffeb+0x20" 1
        . action "xdotool key 0xffeb+0xffe1+0x20" 2
        . action "xdotool key 0xffeb+0xffe3+0x20" 3
        . action "xdotool key 0xffeb+0xff09" 5
        . fc (XwmTheme.cyan XwmTheme.palette) ""
    , ppOrder            = \[ws, l, t, ex] -> [ws, l, ex, t]
    , ppSort             = getSortByIndex
    , ppExtras           = [windowCount]
    , ppOutput           = putStrLn
    }

xBarConfig :: StatusBarConfig
xBarConfig = statusBarProp xbarCmd $ clickablePP $ filterOutWsPP [scratchpadWorkspaceTag] xBarPP
  where
    xbarCmd = unwords ["xbar", flagIconRoot]
    flagIconRoot = "--iconroot=" <> rootdir <> "/icons"
    rootdir = "\"${XMONAD_CONFIG_DIR}\""
