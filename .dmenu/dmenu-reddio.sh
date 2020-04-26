#!/bin/bash

declare -a options=("r/archlinux
r/bash
r/commandline
r/i3wm
r/linux
r/archlinux
r/linux4noobs
r/linuxmasterrace
r/linuxquestions
r/suckless
r/unixporn
r/vim
r/linuxmemes
r/linuxhardware
quit")

choice=$(echo -e "${options[@]}" | dmenu -l 10  -i -p 'Last 10 Posts From Reddit: ')

case "$choice" in
	quit)
		echo "Program terminated." && exit 1
	;;
	r/*)
		reddio print -l 15 "$choice" | sed 's/\x1b\[[0-9;]*m//g' \
			| dmenu -l 15 -i -p "$choice"
		exec st -e $SHELL -c "reddio print -l 10 $choice;$SHELL"
	;;
	*)
		exit 1
	;;
esac
