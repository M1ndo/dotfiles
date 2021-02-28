#!/bin/bash
IMG_PATH=/home/ybenel/Pictures/Screenshots
UL=fb
EDIT=gimp
TIME=3000 


prog="
---Local screenshots (saved at IMG_PATH)---
1.quick_fullscreen
2.delayed_fullscreen
3.section
4.edit_fullscreen
---Screenshot copied to clipboard---
a.upload_fullscreen
u.upload_delayed_fullscreen
e.edit_upload_fullscreen
s.upload_section
p.edit_upload_section
"

cmd=$(dmenu  -l 20  -nf '#999' -nb '#292d3e' -sf '#eee' -sb '#0077bb' -p 'Choose Screenshot Type'   <<< "$prog")

cd $IMG_PATH
case ${cmd%% *} in

	1.quick_fullscreen)	scrot -d 1 'ybenel@%H-%M-%S-scrot.png'  && notify-send -u low -t $TIME 'Scrot' 'Fullscreen saved'  ;;
	2.delayed_fullscreen)	scrot -d 4 'ybenel@%H-%M-%S-scrot.png'  && notify-send -u low -t $TIME 'Scrot' 'Fullscreen saved'    ;;
	3.section)	scrot -a $(slop -f '%x,%y,%w,%h') -d 2 && notify-send -u low -t $TIME 'Scrot' 'Section Screen saved'    ;;
	4.edit_fullscreen)	scrot -d 1 'ybenel@%H-%M-%S-scrot.png' -e "$EDIT \$f"  && notify-send -u low -t $TIME 'Scrot' 'Screenshot edited and saved' ;;

	a.upload_fullscreen)	scrot -d 1 'ybenel@%H-%M-%S-scrot.png' -e "$UL \$f" && (xclip -o;echo) | xclip -selection clipboard  && notify-send -u low -t $TIME "Scrot" "Screenshot Uploaded (powered by fb) - $(xclip -o;echo)"  ;;
  u.upload_delayed_fullscreen)	scrot -d 4 'ybenel@%H-%M-%S-scrot.png' -e "$UL \$f"  && (xclip -o;echo) | xclip -selection clipboard  && notify-send -u low -t $TIME "Scrot" "Screenshot Uploaded (powered by fb) - $(xclip -o)"  ;;
	e.edit_upload_fullscreen)	scrot -d 4 'ybenel@%H-%M-%S-scrot.png' -e "$EDIT \$f && $UL \$f && rm -f \$f"  && notify-send -u low -t $TIME "Scrot" "Screenshot Uploaded (powered by fb) - $(xclip -o)"  ;;
	s.upload_section)	scrot -a $(slop -f '%x,%y,%w,%h') -d 2 -e "$UL \$f"  && (xclip -o;echo) | xclip -selection clipboard   &&  notify-send -u low -t $TIME "Scrot" "Screenshot Uploaded (powered by fb - $(xclip -o)";;
  p.edit_upload_section)  scrot -a $(slop -f '%x,%y,%w,%h') -d 2 -e "$EDIT \$f && $UL \$f && rm -f \$f"  && (xclip -o;echo) | xclip -selection clipboard && notify-send -u low -t $TIME "Scrot" "Screenshot Uploaded (powered by FB) - $(xclip -o)"  ;;


  	*)		exec "'${cmd}'"  ;;
esac
