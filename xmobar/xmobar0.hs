Config { font = "Roboto Bold 10"
       , additionalFonts =
          [ "FontAwesome 12"
          , "FontAwesome Bold 8"
          , "FontAwesome 14"
          , "Hack 19"
          , "Hack 14"
          ]
       , border = NoBorder
       , bgColor = "#2B2E37"
       , fgColor = "#929AAD"
       , alpha = 255
       , position = TopSize L 100 40
       -- , textOffset = 24
       -- , textOffsets = [ 25, 24 ]
       , lowerOnStart = True
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , iconRoot = "/usr/local/etc/xmobar/weather/icons/32x32"
       , commands =
         [ Run UnsafeXPropertyLog "_XMONAD_LOG_0"
         , Run Date "%a, %d %b %Y # %H:%M:%S" "date" 10
         , Run Memory ["-t","Mem: <fc=#AAC0F0><usedratio></fc>%"] 10
         , Run Com "xmbr-cpu-temp" [] "cpu" 10
         , Run Com "xmbr-avail-updates" [] "updates" 10
         , Run Com "xmbr-volume" [] "volume" 10
         , Run Com "xmbr-bluetooth" [] "bluetooth" 10
         , Run Com "xmbr-wifi.sh" [] "network" 10
         , Run Com "xmbr-weather" [] "weather" 9000
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "\
            \    \
            \%_XMONAD_LOG_0%\
            \}\
            \<action=xdotool key super+r>%date%</action>\
            \{\
            \<action=setsid -f $TERMINAL -e ~/Scripts/pop_upgrade.sh>%updates%</action>\
            \<action=xdotool key super+y>\
            \     \
            \%memory%\
            \     \
            \|\
            \     \
            \%cpu%\
            \     \
            \|\
            \     \
            \%weather%\
            \       \
            \</action>"
       }
