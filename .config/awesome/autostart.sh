#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}
#run "xrandr --output VGA-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal"
#run "xrandr --output HDMI2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off"
run "picom"
run "nm-applet"
#run "caffeine"
run "xfce4-power-manager"
run "blueberry-tray"
run "/usr/lib/polkit-kde-authentication-agent-1"
run "/usr/bin/emacs --daemon &"
run "numlockx on"
run "unclutter -root"
run "volumeicon"
#run "/home/ybenel/monitors.sh"
#run "/home/ybenel/.bin/ybl/jack_start"
run "nitrogen --restore"
run "xscreensaver -no-splash"
#run "buckle -f -g 40"

#run applications from startup
#run "firefox"
#run "atom"
#run "dropbox"
#run "insync start"
#run "spotify"
