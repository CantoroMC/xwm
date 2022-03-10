module XMonad.User.Layout.Layouts
    (
    -- applySpacing
    -- , xwmDecorationTheme
    xwmTall
    , xwmThreeCol
    -- , xwmTatami
    -- , xwmFloat
    , xwmTwoPane
    ) where

import XMonad
-- Layout Modifiers
import           XMonad.Layout.LayoutModifier        ( ModifiedLayout )
import           XMonad.Layout.Renamed               ( Rename(Replace,CutWordsLeft), renamed )
-- Layouts
import           XMonad.Layout.ResizableThreeColumns ( ResizableThreeCol(ResizableThreeColMid) )
import           XMonad.Layout.ResizableTile         ( ResizableTall(ResizableTall) )
import           XMonad.Layout.TwoPanePersistent     ( TwoPanePersistent(..) )


xwmTall :: ModifiedLayout Rename ResizableTall Window
xwmTall = renamed [Replace "Tall"] $ ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2

xwmThreeCol :: ModifiedLayout Rename ResizableThreeCol Window
xwmThreeCol =
    renamed [Replace "ThreeCol"] $ ResizableThreeColMid nmaster delta ratio []
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2

xwmTwoPane :: ModifiedLayout Rename TwoPanePersistent Window
xwmTwoPane = renamed [Replace "TwoPane"] $ TwoPanePersistent Nothing delta ratio
  where
    delta   = 3 / 100
    ratio   = 1 / 2
