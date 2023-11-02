#!/bin/bash

case "$1" in
  pre)
    systemctl stop remap-keys.service
    ;;
  post)
    systemctl start remap-keys.service
    ;;
  *)
    ;;
esac
