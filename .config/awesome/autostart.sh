#!/usr/bin/env bash

function run { 
    CMD=$(echo $1 | awk '{print $1}')
    if [[ -z $(pgrep $CMD) ]]; then
        echo "This doesn't have sxhkd";
        $1 &
    fi
}
run "picom"
run "nm-applet"
run "nitrogen --restore"
run "numlockx on"
#run "xfce4-power-manager"
run "/usr/lib/polkit-kde-authentication-agent-1"
#run "/usr/bin/emacs --daemon"
run "xscreensaver -no-splash"
run "caffeine"
run "unclutter -root"
run "volumeicon"
run "mpd"
