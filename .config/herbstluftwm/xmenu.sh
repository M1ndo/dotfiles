#!/bin/sh

cat <<EOF | xmenu | sh &
IMG:./icons/chrm.png	Web Browser	google-chrome-stable
IMG:./icons/code.png	Terminal	xterm
IMG:./icons/shutdown.png	Shutdown	systemctl -i poweroff
IMG:./icons/restart.png	Restart	systemctl -i reboot
IMG:./icons/hibernate.png	Hibernate	systemctl -i hibernate
IMG:./icons/sleep.png	Sleep	systemctl -i suspend
EOF

# Applications
# 	IMG:./icons/web.png	Web Browser	firefox
# 	IMG:./icons/gimp.png	Image editor	gimp
# Terminal (xterm)	xterm
# Terminal (urxvt)	urxvt
# Terminal (st)		st

# Shutdown		poweroff
# Reboot			reboot
