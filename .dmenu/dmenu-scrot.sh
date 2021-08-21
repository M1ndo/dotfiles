#!/usr/bin/env bash
#
# Script name: dm-maim
# Description: Choose type of screenshot to take with maim.
# Dependencies: dmenu, maim, tee, xdotool, xclip, xrandr
# GitLab: https://www.gitlab.com/dwt1/dmscripts
# License: https://www.gitlab.com/dwt1/dmscripts/LICENSE
# Contributors: Derek Taylor
#               Simon Ingelsson
#               Steven Hall


# Set with the flags "-e", "-u","-o pipefail" cause the script to fail
# if certain things happen, which is a good thing.  Otherwise, we can
# get hidden bugs that are hard to discover.
set -euo pipefail
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

get_timestamp() {
  date '+%Y%m%d-%H%M%S'
}


main() {
  local _maim_args=""
  local _file_type=""
  # Makes sure the directory exists.
  # shellcheck disable=SC2154
  mkdir -p "${maim_dir}"

  declare -a modes=(
  "Fullscreen"
  "Active window"
  "Selected region"
  "Region Delay"
  )
  # Get monitors and their settings for maim
  _displays=$(xrandr --listactivemonitors | grep '+' | awk '{print $4, $3}' | awk -F'[x/+* ]' '{print $1,$2"x"$4"+"$6"+"$7}')


  # Add monitor data
  IFS=$'\n'
  declare -A _display_mode
  for i in ${_displays}; do
    name=$(echo "${i}" | awk '{print $1}')
    rest="$(echo "${i}" | awk '{print $2}')"
    modes[${#modes[@]}]="${name}"
    _display_mode[${name}]="${rest}"
  done
  unset IFS

  target=$(printf '%s\n' "${modes[@]}" | ${DMENU} -i -l 20 -p 'Take screenshot of:' "$@") || exit 1
  case "$target" in
    'Fullscreen')
      _file_type="full"
    ;;
    'Active window')
      active_window=$(xdotool getactivewindow)
      _maim_args="-i ${active_window}"
      _file_type="window"
    ;;
    'Selected region')
      _maim_args="-s"
      _file_type="region"
    ;;
    'Region Delay')
      _maim_args="-d 5 -s"
      _file_type="region"
    ;;
    *)
      _maim_args="-g ${_display_mode[${target}]}"
      _file_type="${target}"
    ;;
  esac

  _maim_args="${_maim_args} -q"
  local destination=( "File" "Clipboard" "Both" )
  dest=$(printf '%s\n' "${destination[@]}" | ${DMENU} -i -l 20 -p 'Destination:' "$@" ) || exit 1
  case "$dest" in
    'File')
      # shellcheck disable=SC2086,SC2154
      maim ${_maim_args} "${maim_dir}/${maim_file_prefix}-${_file_type}-$(get_timestamp).png"
      notify-send "Saved Screenshot" "${maim_dir}/${maim_file_prefix}-${_file_type}-$(get_timestamp).png"
    ;;
    'Clipboard')
      # shellcheck disable=SC2086
      echo maim ${_maim_args} | xclip -selection clipboard -t image/png
      notify-send "Saved Screenshot" "Clipboard"
    ;;
    'Both')
      # shellcheck disable=SC2086
      maim ${_maim_args} | tee "${maim_dir}/${maim_file_prefix}-${_file_type}-$(get_timestamp).png" | xclip -selection clipboard -t image/png
      notify-send "Saved Screenshot" "${maim_dir}/${maim_file_prefix}-${_file_type}-$(get_timestamp).png And Clipboard"
    ;;
    *)
      exit 1
    ;;
  esac

}

[[ "${BASH_SOURCE[0]}" == "${0}" ]] && main "$@"
