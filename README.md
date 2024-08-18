# dotfiles
Project with set of files used as dotfiles.

## XMonad
description: configuration for XMonad Window Manager
dir:    .xmonad
files:
    xmonad.hs
path to put: $USER_HOME
setup results:
    ~/.xmonad
    ~/.xmonad/xmonad.hs
usage:
    compilation: sequence of commands
        $ cd ~/.xmonad
        $ xmonad --recompile
        $ xmonad --restart
important notes:
    may need recompilation after each file update!
    may need recompilation after each xmonad package update!

## XMoBar
description: configuration for XMoBar - the best Status Bar for XMonad
dir:    xmobar
files:
    xmobar0.hs
    xmobar1.hs
path to put: $USER_HOME/.config/xmonad/xmobar
setup results:
    ~/.config/xmonad/xmobar
    ~/.config/xmonad/xmobar/xmobar0.hs
    ~/.config/xmonad/xmobar/xmobar1.hs
usage: is invoked automatically by xmobar - if is properly configured

## XMoBar Weather
description: datum for XMoBar Weather indicator // by default only on xmobar0.hs
dir:    weather
files:
    data/weather_conditions.json
    icons/32x32/*.xpm
path to put: /usr/local/etc/xmobar/weather
setup results:
    /usr/local/etc/xmobar/weather
    /usr/local/etc/xmobar/weather/data
    /usr/local/etc/xmobar/weather/data/weather_conditions.json
    /usr/local/etc/xmobar/weather/icons
    /usr/local/etc/xmobar/weather/icons/32x32
    /usr/local/etc/xmobar/weather/icons/32x32/*.xpm
usage: used by script xmbr-weather.bash

## help
Guides on different topics

## bin
description: some useful scripts for user
dir: bin
path to put: /usr/local/bin



