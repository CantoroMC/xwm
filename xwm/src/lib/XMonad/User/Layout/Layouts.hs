module XMonad.User.Layout.Layouts
    (
    xwmTall
    , xwmThreeCol
    , xwmTwoPane
    , xwmGrid
    , xwmFloat
    , xwmOneBig
    -- , xwmDecorationTheme
    , applySpacing
    ) where

import XMonad
-- Layout Modifiers
import           XMonad.Layout.LayoutModifier        ( ModifiedLayout )
import           XMonad.Layout.NoBorders             ( SmartBorder, smartBorders, WithBorder, noBorders )
import           XMonad.Layout.Renamed               ( Rename(Replace, CutWordsLeft), renamed )
import           XMonad.Layout.Spacing               ( Border(..), Spacing(..), spacingRaw )
import           XMonad.Layout.WindowArranger        ( WindowArranger )
-- Layouts
import           XMonad.Layout.Grid                  ( Grid(..) )
import           XMonad.Layout.OneBig
import           XMonad.Layout.ResizableThreeColumns ( ResizableThreeCol(ResizableThreeColMid) )
import           XMonad.Layout.ResizableTile         ( ResizableTall(ResizableTall) )
import           XMonad.Layout.SimplestFloat         ( SimplestFloat, simplestFloat )
import           XMonad.Layout.TwoPanePersistent     ( TwoPanePersistent(..) )

xwmTall :: ModifiedLayout SmartBorder (ModifiedLayout Rename ResizableTall) Window
xwmTall =
    smartBorders
    . renamed [Replace "Tall"]
    $ ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2

xwmThreeCol :: ModifiedLayout SmartBorder (ModifiedLayout Rename ResizableThreeCol) Window
xwmThreeCol =
    smartBorders
    . renamed [Replace "ThreeCol"]
    $ ResizableThreeColMid nmaster delta ratio []
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 3

xwmGrid :: ModifiedLayout SmartBorder (ModifiedLayout Rename Grid) Window
xwmGrid = smartBorders $ renamed [Replace "Grid"] Grid

xwmTwoPane :: ModifiedLayout SmartBorder (ModifiedLayout Rename TwoPanePersistent) Window
xwmTwoPane =
    smartBorders
    . renamed [Replace "TwoPane"]
    $ TwoPanePersistent Nothing delta ratio
  where
    delta   = 3 / 100
    ratio   = 1 / 2

xwmFloat :: ModifiedLayout WithBorder (ModifiedLayout Rename (ModifiedLayout WindowArranger SimplestFloat)) Window
xwmFloat =
    noBorders
    . renamed [Replace "Float"]
    $ simplestFloat

xwmOneBig :: ModifiedLayout SmartBorder (ModifiedLayout Rename OneBig) Window
xwmOneBig =
    smartBorders
    . renamed [Replace "OneBig"]
    $ OneBig width height
  where
    width   = 3 / 4
    height   = 3 / 4

applySpacing
    :: Integer
    -> l Window
    -> ModifiedLayout Rename (ModifiedLayout Spacing l) Window
applySpacing sz =
    renamed [CutWordsLeft 1]
        . spacingRaw False (Border sz sz sz sz) True (Border sz sz sz sz) True
