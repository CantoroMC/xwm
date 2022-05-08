{-# LANGUAGE FlexibleContexts #-}

module XMonad.User.Manage.Hook
    ( xwmManageHook
    ) where

import           Control.Monad                 ( liftM2 )
import           Data.Maybe                    ( isNothing )
import           Text.Regex                    ( matchRegex, mkRegex )
import           XMonad
import qualified XMonad.StackSet               as XMSS
import           XMonad.Hooks.ManageDocks      ( manageDocks )
import           XMonad.Hooks.ManageHelpers
    ( doCenterFloat
    , doRectFloat
    , (-?>)
    , isDialog
    , transience
    , composeOne
    )
import           XMonad.Util.NamedScratchpad   ( namedScratchpadManageHook )

import           XMonad.User.Layout.Workspaces ( xwmWorkspaces )
import           XMonad.User.Manage.Util       ( xwmBigRect, xwmMedRect, xwmSideLeft, xwmSPDs )



role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- name :: Query String
-- name = stringProperty "WM_NAME"

(*!=) :: String -> String -> Bool
q *!= x = isNothing $ matchRegex (mkRegex x) q

(*!?) :: Functor f => f String -> String -> f Bool
q *!? x = fmap (*!= x) q


manageOthers :: ManageHook
manageOthers = composeAll
    [ -- doShift
      className =? "Transmission-gtk"             --> doShift (xwmWorkspaces !! 8)
    , className =? "MATLAB R2021a - academic use" --> doShift (xwmWorkspaces !! 1)
    , className =? "mpv"                          --> doShift (xwmWorkspaces !! 4)
    -- doIgnore
    , resource  =? "trayer" --> doIgnore
    , className =? "Conky" --> doIgnore
    ]
    -- where doShiftAndGo = doF . liftM2 (.) XMSS.greedyView XMSS.shift

manageFloatings :: ManageHook
manageFloatings =
    composeAll $
        [className =? appToFloat --> doCenterFloat | appToFloat <- appsToFloat]
        ++
        [ title =? "Event Tester"  --> doFloat
        , (className =? "MATLAB R2021a - academic use" <&&> title *!?  "^MATLAB") --> doFloat
        , title =? "lstopo"             --> doCenterFloat
        , title =? "Picture in picture" --> doCenterFloat
        , role  =? "pop-up"             --> doCenterFloat
        , title =? "weatherreport"      --> doRectFloat xwmBigRect
        , title =? "keysheet"           --> doRectFloat xwmSideLeft
        , title =? "volume"             --> doRectFloat xwmMedRect
        , (className =? "Display" <&&> title =? "ImageMagick: ") --> doCenterFloat
        ]  where
            appsToFloat =
                [ "Arandr"
                , "Avahi-discover"
                , "Blueberry.py"
                , "Bssh"
                , "Bvnc"
                , "CMakeSetup"
                , "feh"
                , "Hardinfo"
                , "imagewriter"
                , "Lxappearance"
                , "matplotlib"
                , "Nibbler"
                , "Nm-connection-editor"
                , "ParaView"
                , "Parcellite"
                , "Pavucontrol"
                , "qv4l2"
                , "qvidcap"
                , "Sxiv"
                , "System-config-printer.py"
                , "Transmission-gtk"
                , "Xboard"
                , "Xmessage"
                , "Yad"
                , "Yad-icon-browser"
                ]

xwmManageHook :: ManageHook
xwmManageHook =
    composeOne
        [ transience
        , isDialog -?> doCenterFloat
        ]
    <+> namedScratchpadManageHook xwmSPDs
    <+> manageDocks
    <+> manageFloatings
    <+> manageOthers
