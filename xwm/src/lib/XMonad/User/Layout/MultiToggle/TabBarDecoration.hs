{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module XMonad.User.Layout.MultiToggle.TabBarDecoration ( XwmTabBar(..) ) where

import XMonad
import XMonad.Layout.Decoration
    ( ModifiedLayout(..)
    , decoration
    , shrinkText
    , Decoration
    , DefaultShrinker
    )
import XMonad.Layout.MultiToggle  ( Transformer(..) )
import XMonad.Layout.TabBarDecoration
    ( XPPosition(Top)
    , resizeVertical
    , ResizeScreen
    , TabBarDecoration(..)
    )
import XMonad.User.Layout.Decorations ( xwmDecorationTheme )


xwmTabBar
    :: Eq a
    => l a
    -> ModifiedLayout
           (Decoration TabBarDecoration DefaultShrinker)
           (ModifiedLayout ResizeScreen l)
           a
xwmTabBar =
    decoration shrinkText xwmDecorationTheme (TabBar Top) . resizeVertical 15

data XwmTabBar = XWMTABBAR
    deriving (Read, Show, Eq)
instance Transformer XwmTabBar Window where
    transform _ x k =
        k (xwmTabBar x) (\(ModifiedLayout _ (ModifiedLayout _ x')) -> x')
