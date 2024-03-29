#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

MONITOR=$(xrandr --query | grep " connected" | cut -d" " -f1 | head -1) polybar main -c $HOME/.config/polybar/config.ini &

#if type "xrandr"; then
#  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
#    echo "Running"
#    MONITOR=$m polybar main -c $HOME/.config/polybar/config.ini &
#  done
#else
#  echo "Running2"
#  polybar main -c $HOME/.config/polybar/config.ini &
#fi

echo "Bars launched..."
