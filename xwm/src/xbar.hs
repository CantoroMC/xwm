import Xmobar
    ( xmobar
    , Align(C)
    , Border(NoBorder)
    , Config(..)
    , XPosition(TopSize)
    , XMonadLog(UnsafeXMonadLog)
    , Runnable(Run)
    , defaultConfig
    )

import qualified Theme.Palette        as XbarTheme
import           Xmobar.User.Util     ( action, fn, rootdir, sep )
import           Xmobar.User.Monitors
    ( pacman
    , pomodoro
    , sound
    , battery
    , brightness
    , dynNet
    , memory
    , mpdMusic
    , multicpu
    , swap
    , thermal
    , uptime
    , weather
    , clock
    , keyboard
    , trayerPad
    , xBarMenu
    )

xbarConfig :: Config
xbarConfig = defaultConfig
    { font             =  "Operator Mono Lig Nerd Font Italic 8"
    , additionalFonts  =
        [ "FiraCode Nerd Font 7" -- for the win titles
        , "Operator Mono Lig Nerd Font 7"
        , "FiraCode Nerd Font 8" -- for the workspaces
        , "FiraCode Nerd Font 12"
        ]
    , bgColor          = XbarTheme.background0 XbarTheme.palette
    , fgColor          = XbarTheme.foreground0 XbarTheme.palette
    , alpha            = 255
    , position         = TopSize C 100 22
    , border           = NoBorder
    , borderColor      = XbarTheme.background1 XbarTheme.palette
    , borderWidth      = 2
    , textOffset       = -1
    , textOffsets      = [-1, -1, -1, -1]
    , iconOffset       = -1
    , hideOnStart      = False
    , lowerOnStart     = True
    , persistent       = False
    , allDesktops      = True
    , overrideRedirect = True
    , pickBroadest     = False
    , iconRoot         = rootdir <> "/icons"
    , commands         =
        [ Run UnsafeXMonadLog
        , Run trayerPad
        , Run pacman
        , Run pomodoro
        , Run sound
        , Run (battery "BAT0" "battery0")
        , Run brightness
        , Run dynNet
        , Run memory
        , Run mpdMusic
        , Run multicpu
        , Run swap
        , Run thermal
        , Run uptime
        , Run weather
        , Run clock
        , Run keyboard
        ]
    , sepChar          = "|"
    , alignSep         = "}{"
    , template         = xBarTemplate
    , verbose          = False
    }

xBarTemplate :: String
xBarTemplate =
    archMenu
    ++ sep
    ++ "|UnsafeXMonadLog|"
    ++ sep
    ++ pymodoro
    ++ sep
    ++ "|music|"
    ++ sep
    ++ update
    ++ " "
    ++ "|uptime|"
    ++ " "
    ++ "|kbd|"
    ++ sep
    ++ "}|date|"
    ++ " "
    ++ "|LIML|{"
    ++ sep
    ++ "|memory|(|swap|) |multicoretemp| |multicpu|"
    ++ sep
    ++ "|bright| |battery0|" ++ " " ++ volume
    ++ sep
    ++ "|dynnetwork|"
    ++ sep
    ++ "|trayerPad|"
      where
        volume =
            action   "pactl set-sink-volume @DEFAULT_SINK@ -5%"         5
            $ action "pactl set-sink-volume @DEFAULT_SINK@ +5%"         4
            $ action "pactl set-sink-mute @DEFAULT_SINK@ toggle"        2
            $ action "kitty --name volume --title volume -e pulsemixer" 1
            $ action "pavucontrol"                                      3
            "|sound|"
        update = action "kitty -e sudo pacman -Syu" 3 "|pacman|"
        pymodoro =
            action "touch ~/.cache/pymodoro" 1
            $ action "rm ~/.cache/pymodoro"  3
            $ action "kitty -e nvim ~/.cache/pymodoro" 2
            $ fn 1
            "|pomodoro|"
        archMenu =
            xBarMenu
                "xmenu-apps"
                "xmenu-shutdown"
                "xdotool key 0xffeb+0x79"
                (XbarTheme.blue XbarTheme.palette)
                "\62211"

main :: IO ()
main = xmobar xbarConfig
