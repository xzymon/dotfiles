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
       , lowerOnStart = True
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , iconRoot = "/usr/local/etc/icons/32x32/"
       , commands =
         [ Run UnsafeXPropertyLog "_XMONAD_LOG_1"
         , Run Date "%a, %d %b   <fn=5>󰥔</fn>     %H:%M:%S" "date" 10
         , Run Memory ["-t","Mem: <fc=#AAC0F0><usedratio></fc>%"] 10
         , Run Com "xmbr-cpu-temp" [] "cpu" 10
        -- , Run Com "/home/amnesia/.config/xmonad/xmobar/available_updates.sh" [] "updates" 10
         , Run Com "xmbr-volume" [] "volume" 10
        -- , Run Com "/home/amnesia/.config/xmonad/xmobar/bluetooth.sh" [] "bluetooth" 10
        -- , Run Com "/home/amnesia/.config/xmonad/xmobar/wifi.sh" [] "network" 10
         , Run Com "/home/amnesia/.config/xmonad/xmobar/trayer-padding.sh" [] "trayerpad" 10
        -- network activity monitor (dynamic interface resolution)
        , Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                             , "--Low"      , "1000"       -- units: B/s
                             , "--High"     , "5000"       -- units: B/s
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10 
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "\
            \    \
            \%_XMONAD_LOG_1%\
            \}\
            \<action=xdotool key super+r>%date%</action>\
            \{\
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
            \%volume%\
            \     \
            \|\
            \     \
            \%dynnetwork%\
            \     \
            \</action>\
            \|\
            \   \
            \%trayerpad%"
       }
