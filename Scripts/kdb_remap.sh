#!/bin/bash

export DISPLAY=:0
export XAUTHORITY=/home/alienx/.Xauthority

if [[ $(xinput --list | grep -i "SEMICO USB Keyboard") ]]; then
    sleep 1
    xhost +SI:localuser:root
    su -c "xmodmap /home/alienx/.Xmodmap" alienx
    xhost -SI:localuser:root
else
    exit 0
fi


