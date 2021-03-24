#!/bin/sh

if [ "$(mocp -Q %state)" == "PLAY" ];then
    SONG=$(mocp -Q %song)
    if [ -n "$SONG" ]; then
        echo "$(mocp -Q %artist) - $SONG"
    else
        basename "$(mocp -Q %file)"
    fi
elif [ "$(playerctl -p spotify status)" == "Playing" ]; then
    SONG=$(playerctl -p spotify metadata --format "{{xesam:artist}} - {{xesam:title}}")
    echo $SONG
else
	echo "Nothing Is Playing"
fi
