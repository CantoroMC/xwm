module XMonad.User.Bindings.Mouse ( xwmMouseBindings ) where

import qualified Data.Map as Map (Map, fromList)
import           XMonad
    ( button1
    , button2
    , button3
    , focus
    , mouseMoveWindow
    , mouseResizeWindow
    , windows
    , Button
    , KeyMask
    , Window
    , X
    , XConfig(XConfig, modMask)
    )
import qualified XMonad.StackSet as XMSS

xwmMouseBindings :: XConfig l -> Map.Map (KeyMask, Button) (Window -> X ())
xwmMouseBindings XConfig {XMonad.modMask = modm} = Map.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w   >> windows XMSS.shiftMaster)
    , ((modm, button2), \w -> focus w                        >> windows XMSS.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows XMSS.shiftMaster)
    ]
