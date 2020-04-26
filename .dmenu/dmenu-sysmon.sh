#!/bin/bash

# Dmenu script for launching system monitoring programs.
declare -a options=("htop
glances
gtop
iftop
iotop
iptraf-ng
nmon
s-tui
quit")

choice=$(echo -e "${options[@]}" | dmenu -l -i -p 'System monitors: ')

case $choice in
	quit)
		echo "Program terminated." && exit 1
	;;
	htop| \
	glances| \
	gtop| \
	nmon| \
	s-tui)
        exec st -e $choice
	;;
	iftop| \
	iotop| \
	iptraf-ng)
        exec st -e gksu $choice
	;;
	*)
		exit 1
	;;
esac

