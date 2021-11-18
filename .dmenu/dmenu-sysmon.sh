#!/bin/bash

function get_config() {
  local loaded=0
  declare -a config_dirs=(
  "${HOME}/.dmenu/config"
  "/etc/dmenu/config"
  )
  for conf in "${config_dirs[@]}"; do
    if [[ -f ${conf} ]]; then
      echo "${conf}"
      loaded=1
      break
    fi
  done
  [[ ${loaded} -eq 0 ]] && echo "No config found" ; exit 1
}

# script will not hit this if there is no config-file to load
# shellcheck disable=SC1090
source "$(get_config)"
# Defining our config location
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

choice=$(echo -e "${options[@]}" | ${DMENU} -i -l 20 -p 'System monitors: ')

case $choice in
	quit)
		echo "Program terminated." && exit 1
	;;
	htop| \
	glances| \
	gtop| \
	nmon| \
	s-tui)
        exec xterm -e $choice
	;;
	iftop| \
	iotop| \
	iptraf-ng)
        exec xterm -e gksu $choice
	;;
	*)
		exit 1
	;;
esac

