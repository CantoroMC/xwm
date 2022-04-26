{-# LANGUAGE FlexibleContexts #-}

module XMonad.User.Manage.Util
    ( xwmBigRect
    , xwmMedRect
    , xwmSmallRect
    , xwmUpLeftRect
    , xwmUpRightRect
    , xwmDownLeftRect
    , xwmDownRightRect
    , xwmSideLeft
    , xwmSPDs
    , applyUrgencyHook
    , xwmRescreenCfg
    ) where

import           XMonad
    ( XConfig
    , Window
    , spawn
    , (=?)
    , title
    , Default(def)
    , LayoutClass
    )
import qualified XMonad.StackSet             as XMSS
import           XMonad.Hooks.ManageHelpers  ( doRectFloat )
import           XMonad.Hooks.Rescreen       ( RescreenConfig(afterRescreenHook, randrChangeHook) )
import           XMonad.Hooks.UrgencyHook
    ( BorderUrgencyHook (..)
    , UrgencyConfig (..)
    , SuppressWhen (..)
    , RemindWhen (..)
    , withUrgencyHookC
    )
import           XMonad.Util.NamedScratchpad ( NamedScratchpad(NS), NamedScratchpads )

import qualified Theme.Palette               as XwmTheme


xwmBigRect :: XMSS.RationalRect
xwmBigRect = XMSS.RationalRect 0.166 0.166 0.666 0.666

xwmMedRect :: XMSS.RationalRect
xwmMedRect = XMSS.RationalRect 0.25 0.25 0.5 0.5

xwmSmallRect :: XMSS.RationalRect
xwmSmallRect = XMSS.RationalRect 0.3 0.3 0.3 0.3

xwmUpLeftRect :: XMSS.RationalRect
xwmUpLeftRect = XMSS.RationalRect 0 0.02 0.5 0.49

xwmUpRightRect :: XMSS.RationalRect
xwmUpRightRect = XMSS.RationalRect 0.5 0.02 0.5 0.49

xwmDownLeftRect :: XMSS.RationalRect
xwmDownLeftRect = XMSS.RationalRect 0 0.51 0.5 0.49

xwmDownRightRect :: XMSS.RationalRect
xwmDownRightRect = XMSS.RationalRect 0.5 0.51 0.5 0.49

xwmSideLeft :: XMSS.RationalRect
xwmSideLeft = XMSS.RationalRect 0 0.02 0.4 0.98



xwmSPDs :: NamedScratchpads
xwmSPDs =
    [ NS "Yakuake" "kitty --title Yakuake --name Yakuake"
        (title =? "Yakuake") (doRectFloat xwmMedRect)
    , NS "Kimux" "kitty --title Kimux --name Kimux -e tmux"
        (title =? "Kimux") (doRectFloat xwmMedRect)
    , NS "Ncmpcpp" "kitty --title Ncmpcpp --name Ncmpcpp -e ncmpcpp"
        (title =? "Ncmpcpp") (doRectFloat xwmMedRect)
    , NS "Cmus" "kitty --title Cmus --name Cmus -e cmus"
        (title =? "Cmus") (doRectFloat xwmMedRect)
    , NS "HsCalc" "kitty --title HsCalc --name HsCalc -e ghci"
        (title =? "HsCalc") (doRectFloat xwmMedRect)
    ]

applyUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
applyUrgencyHook = withUrgencyHookC xwmUrgencyHook xwmUrgencyConfig
  where
    xwmUrgencyHook = BorderUrgencyHook
        { urgencyBorderColor = XwmTheme.red XwmTheme.palette
        }
    xwmUrgencyConfig = UrgencyConfig
        { suppressWhen = Focused
        , remindWhen = Repeatedly 3 30
        }

xwmRescreenCfg :: RescreenConfig
xwmRescreenCfg = def
    { afterRescreenHook = spawn "xwm-rescreen"
    , randrChangeHook   = mempty
    }
