#!/usr/bin/env bash

function check_root() {
  if [[ "$(id -u)" -ne 0 ]]; then
    printf "[!] Forgot Sudo ?\n"
    exit 1
  fi
}

trap ctrl_c INT
ctrl_c () {
  printf "[!] Exiting.....\n"
  exit 1
}

upgrade() {
  # Upgrade System Packages.
  yes | pacman -Syu
  if [[ $? -eq 0 ]]; then
      printf "[+] System Upgrade Is Done Successfully.\n"
  else
      printf "[+] System Upgrade Failed Restarting ..\n"
      upgrade
  fi
}

pkginstall() {
  # Install Optional Packages
  pkgs=$1
  for pkg in $pkgs; do
      yes | pacman -S "$pkg"
  done
}

check_root
upgrade
pkginstall "$1"






