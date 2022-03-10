module XMonad.User.Prompt.Config (xwmXpConfig) where

import qualified Data.Map                 as M
import           XMonad
import qualified XMonad.StackSet          as W
import           XMonad.Prompt
    ( vimLikeXPKeymap
    , ComplCaseSensitivity(CaseInSensitive)
    , XPConfig
        ( font
        , bgColor
        , fgColor
        , bgHLight
        , fgHLight
        , borderColor
        , promptBorderWidth
        , position
        , alwaysHighlight
        , height
        , maxComplRows
        , maxComplColumns
        , historySize
        , historyFilter
        , promptKeymap
        , completionKey
        , changeModeKey
        , defaultText
        , autoComplete
        , showCompletionOnTab
        , complCaseSensitivity
        , searchPredicate
        , defaultPrompter
        , sorter
        )
    , XPPosition(Top) )
import           XMonad.Prompt.FuzzyMatch ( fuzzyMatch, fuzzySort )
import qualified Theme.Palette            as XwmTheme

xwmXpConfig :: XPConfig
xwmXpConfig = def
    { font                 = "xft:Operator Mono Lig Nerd Font:pixelsize=12"
    , bgColor              = XwmTheme.background0 XwmTheme.palette
    , fgColor              = XwmTheme.foreground0 XwmTheme.palette
    , bgHLight             = XwmTheme.background1 XwmTheme.palette
    , fgHLight             = XwmTheme.cyan        XwmTheme.palette
    , borderColor          = XwmTheme.background1 XwmTheme.palette
    , promptBorderWidth    = 0
    , position             = Top
    , alwaysHighlight      = True
    , height               = 24
    , maxComplRows         = Just 3
    , maxComplColumns      = Nothing
    , historySize          = 256
    , historyFilter        = id
    , promptKeymap         = vimLikeXPKeymap
    , completionKey        = (0,xK_Tab)
    , changeModeKey        = xK_grave
    , defaultText          = []
    , autoComplete         = Nothing
    , showCompletionOnTab  = False
    , complCaseSensitivity = CaseInSensitive
    , searchPredicate      = fuzzyMatch
    , defaultPrompter      = id
    , sorter               = fuzzySort
    }
