#!/bin/bash
rand=$(shuf -i 1-189573 -n 1)
tmp=$(mktemp /tmp/${rand}.XXXXXXXX.mod)
curl https://modarchive.org/jsplayer.php?moduleid=${rand} > ${tmp}
xmp ${tmp}
rm ${tmp}
