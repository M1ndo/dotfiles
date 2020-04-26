#!/bin/bash

# Dmenu script for launching trading programs.
declare -a options=("tastyworks
tastytrade
thinkorswim
quit")

choice=$(echo -e "${options[@]}" | dmenu -l -i -p 'System monitors: ')

case $choice in
	quit)
		echo "Program terminated." && exit 1
	;;
	tastyworks)
        exec /opt/tastyworks/tastyworks
	;;
	tastytrade)
        exec firefox tastytrade.com
	;;
	thinkorswim)
        exec "$HOME/thinkorswim/thinkorswim"
	;;
	*)
		exit 1
	;;
esac

