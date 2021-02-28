#!/bin/sh

cat <<EOF | xmenu | sh &
IMG:./icons/browser.png	Browser	google-chrome-stable
IMG:./icons/stremio.png	Stremio	stremio
IMG:./icons/discord.png	Discord	Discord
IMG:./icons/atom.png	Atom	atom
IMG:./icons/gimp.png	Gimp	gimp
IMG:./icons/spotify.png Spotify spot_load
IMG:./icons/pcmanfm.png PCManFM pcmanfm
IMG:./icons/telegram.png	Telegram	telegram-desktop
IMG:./icons/xterm.png	Terminal	xterm

IMG:./icons/sleep.png	Sleep	xscreensaver-command -lock
IMG:./icons/hibernate.png	Hibernate	systemctl -i hibernate
IMG:./icons/suspend.png Suspend systemctl -i suspend
IMG:./icons/restart.png	Restart	systemctl -i reboot
IMG:./icons/shutdown.png	Shutdown	shutdown -P now	
EOF

# Applications
# 	IMG:./icons/web.png	Web Browser	firefox
# 	IMG:./icons/gimp.png	Image editor	gimp
# Terminal (xterm)	xterm
# Terminal (urxvt)	urxvt
# Terminal (st)		st

# Shutdown		poweroff
# Reboot			reboot
