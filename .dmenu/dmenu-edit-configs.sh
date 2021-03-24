#!/bin/bash
# Dmenu script for editing some of my more frequently edited config files.
declare options=("awesome
bash
herbstluftwm
emacs
picom
neovim
polybar
st
termite
vifm
xmobar
xmonad
xresources
quit")

choice=$(echo -e "${options[@]}" | dmenu -i -p 'Edit config file: ')

case "$choice" in
	quit)
		echo "Program terminated." && exit 1
	;;
	awesome)
		choice="$HOME/.config/awesome/rc.lua"
	;;
	bash)
		choice="$HOME/.bashrc"
	;;
	herbstluftwm)
		choice="$HOME/.config/herbstluftwm/autostart"
	;;
	emacs)
		choice="$HOME/.doom.d/config.el"
	;;
	picom)
		choice="$HOME/.config/picom/picom.conf"
	;;
	neovim)
		choice="$HOME/.config/nvim/init.vim"
	;;
	polybar)
		choice="$HOME/.config/polybar/config"
	;;
	st)
		choice="$HOME/Public/Dev/st/config.h"
	;;
	termite)
		choice="$HOME/.config/termite/config"
	;;
	vifm)
		choice="$HOME/.config/vifm/vifmrc"
	;;
	xmobar)
		choice="$HOME/.config/xmobar/xmobarrc"
	;;
	xmonad)
		choice="$HOME/.xmonad/xmonad.hs"
	;;
	xresources)
		choice="$HOME/.Xresources"
	;;
	*)
		exit 1
	;;
esac
xterm -e vim "$choice"
