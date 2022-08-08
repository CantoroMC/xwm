module Xmobar.User.Monitors
    (   -- * Commands
        -- $commands
      trayerPad
    , pacman
    , pomodoro
    , sound
        -- * Monitors
        -- $monitors
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
        -- * Others
        -- $others
    , clock
    , keyboard
    , xBarMenu
    ) where

import           Xmobar
    ( CommandReader(..)
    , Date(..)
    , Kbd(..)
    , Alias
    , Monitors
        (WeatherX
        , BatteryN
        , Brightness
        , DynNetwork
        , Memory
        , MPDX
        , MultiCpu
        , Swap
        , MultiCoreTemp
        , Uptime
        )
    , Command(Com)
    )
import qualified Theme.Palette as XbarTheme
import           Xmobar.User.Util
    ( rootdir
    , action
    , fc
    , fn
    , border
    , icon
    , withHighArgs
    , withLowArgs
    )

-------------------------------------------------------------------------------
    -- Commands
trayerPad :: Command
trayerPad =
    Com "/bin/sh" ["-c", rootdir ++ "/scripts/padding-icon"] "trayerPad" 600

pacman :: Command
pacman = Com "/bin/sh" ["-c", rootdir ++ "/scripts/xmPacman"] "pacman" 600

sound :: Command
sound = Com "/bin/sh" ["-c", rootdir ++ "/scripts/xmVolume"] "sound" 10

pomodoro :: CommandReader
pomodoro = CommandReader "pymodoro" "pomodoro"

-------------------------------------------------------------------------------
    -- Monitors
battery :: String -> Alias -> Monitors
battery b = BatteryN [b]
    ( withLowArgs
        [ "--template", "<leftipat> <acstatus>"
        , "--suffix", "True"
        ]
        [ "--on-icon-pattern",   icon "battery/on/battery_on_%%.xpm"
        , "--off-icon-pattern",  icon "battery/off/battery_off_%%.xpm"
        , "--idle-icon-pattern", icon "battery/idle/battery_idle_%%.xpm"
        , "-o" , fn 2 "<left> (<timeleft>)"
        , "-O" , fn 2 "<left> (<timeleft>)"
        , "-i" , fn 2 "IDLE <left>"
        ]
    ) 400

brightness :: Monitors
brightness = Brightness
    (withLowArgs
        [ "--template",
            action "xbacklight -inc 5" 4
            $ action "xbacklight -dec 5" 5
            $ action "xbacklight -set 100" 3
            $ action "xbacklight -set 0" 2
            "<ipat> <percent>"
        , "--suffix", "True"
        ]
        [ "-D", "intel_backlight"
        , "--brightness-icon-pattern", icon "brightness/brightness_%%.xpm"
        ]
    ) 10

dynNet :: Monitors
dynNet = DynNetwork
    ( withHighArgs
        [ "--template",
            fn 2 "<txipat><rxipat> <dev> <tx> <rx>"
        , "--suffix", "True"
        , "--High", "2000"
        , "--Low",  "20"
        , "--ddigits", "1"
        ]
        [ "--rx-icon-pattern", icon "network/rx/network_rx_%%.xpm"
        , "--tx-icon-pattern", icon "network/tx/network_tx_%%.xpm"
        ]
    ) 10

memory :: Monitors
memory = Memory
    ( withHighArgs
        [ "--template",
            action "kitty -e htop" 3
                $ "<usedipat>" ++  fn 2 " <usedratio>"
        , "--ppad", "2"
        , "--suffix", "True"
        ]
        ["--used-icon-pattern", "<icon=ram/ram_%%.xpm/>"]
    ) 10

mpdMusic :: Monitors
mpdMusic = MPDX
    (withHighArgs
        [ "--template", "<statei>"
        , "--maxwidth", "12"
        ]
        [ "-P",
            action "wmctrl -xR ncmpcpp" 3 (fn 2 "<artist>-<title>")
            ++ action "mpc seek +1%" 4
                (action "mpc seek -1%" 5 $ fn 2 " [<lapsed>/<length>]")
            ++ action "mpc volume +3" 4
                (action "mpc volume -3" 5 $ fn 3 " 墳" ++ fn 2 " <volume>%")
        , "-Z",
            action "wmctrl -xR ncmpcpp" 3 (fn 2 "<artist>-<title>")
            ++ action "mpc seek +1%" 4
                (action "mpc seek -1%" 5 $ fn 2 " [<lapsed>/<length>]")
            ++ action "mpc volume +3" 4
                (action "mpc volume -3" 5 $ fn 3 " 墳" ++ fn 2 " <volume>%")
        , "-S" ,
            action "mpc play" 1 $ fn 3 "\63622 "
        ]
    ) 10 "music"

multicpu :: Monitors
multicpu = MultiCpu
    ( withHighArgs
        [ "--template",
            "<ipat> "
            ++ fn 2 "<total>"
        , "--suffix",   "True"
        , "--ppad",     "3"
        , "--ddigits",  "0"
        , "--minwidth", "4"
        , "--align" ,   "l"
        ]
        [ "--load-icon-pattern", icon "cpu/cpu_%%.xpm"
        ]
    ) 10

swap :: Monitors
swap = Swap
    [ "--template", fn 2 "<usedratio>"
    , "--ppad", "2"
    , "--suffix", "True"
    ] 10

thermal :: Monitors
thermal = MultiCoreTemp
    ( withHighArgs
        [ "--template", "<maxipat> " ++ fn 2 "<max>°C"
        ]
        [ "--max-icon-pattern", icon "temperature/temperature_%%.xpm"
        , "--mintemp", "55"
        , "--maxtemp", "100"
        ]
    ) 50

uptime :: Monitors
uptime = Uptime
    [ "--template", fn 3 "羽" ++ fn 1 "<hours>:<minutes>"
    , "--width", "3"
    , "--suffix", "True"
    ] 60

weather :: Monitors
weather = WeatherX
    "LIML"
    [ ("clear"                  , icon "weather/weather_sunny.xpm")
    , ("mostly clear"           , icon "weather/weather_mostly_sunny.xpm")
    , ("sunny"                  , icon "weather/weather_sunny.xpm")
    , ("mostly sunny"           , icon "weather/weather_mostly_sunny.xpm")
    , ("partly sunny"           , icon "weather/weather_mostly_cloudy.xpm")
    , ("cloudy"                 , icon "weather/weather_cloudy.xpm")
    , ("mostly cloudy"          , icon "weather/weather_mostly_cloudy.xpm")
    , ("partly cloudy"          , icon "weather/weather_mostly_sunny.xpm")
    , ("fair"                   , icon "weather/weather_sunny.xpm")
    , ("overcast"               , icon "weather/weather_cloudy.xpm")
    , ("considerable cloudiness", icon "weather/weather_cloudy.xpm")
    , ("obscured"               , icon "weather/weather_obscured.xpm")
    ]
    [ "--template" ,
        "<skyConditionS>"
        ++ action "weather" 3 (
            border "Bottom" (XbarTheme.bGreen XbarTheme.palette) 4 $
                " <weather> <tempC>°C "
                ++ fc (XbarTheme.cyan XbarTheme.palette) "" "<rh>% "
                ++  fn 3 "\57982 "
                ++ "<windKmh>km/h"
        )
    ]
    100

-------------------------------------------------------------------------------
    -- Others
clock :: Date
clock = Date
    (border "Bottom" (XbarTheme.blue XbarTheme.palette) 4 " %T - %a %e %b")
    "date"
    10

keyboard :: Kbd
keyboard = Kbd
    [ ("us",          fn 4 "\63506" ++ fn 1 " US")
    , ("it(winkeys)", fn 4 "\63506" ++ fn 1" IT")
    , ("de(qwerty)",  fn 4 "\63506" ++ fn 1" DE")
    ]

xBarMenu :: String -> String -> String -> String -> String -> String
xBarMenu mp1 mp2 mp3 color ico =
    action mp1 1 $ action mp2 2 $ action mp3 3 $ fn 4 $ fc color "" (ico ++ " ")
