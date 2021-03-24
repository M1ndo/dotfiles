#!/bin/bash
FOLDER=~/dotfiles
function dot_files() {
  if [ -f dot_dir.txt ]; then
    rm dot_dir.txt id_dot.txt root_dir.txt id_root.txt
  fi
  echo $FOLDER
  find $FOLDER -type f -name ".*" | sed '3d'|sed '10d' | xargs md5sum 
  find $FOLDER -type f -name ".*" | sed '3d'|sed '10d' | xargs md5sum | awk '{print $1 >> "dot_dir.txt"; print $2 >> "id_dot.txt"}'
  cut -d"/" -f 5 id_dot.txt | while read file; do
    md5sum ~/$file | awk '{print $1 >> "root_dir.txt"; print >> "id_root.txt"}'
  done
}

function read_comp() {
  while
    read check &&
    read check2 <&3
  do
  if [ "$check2" != "$check" ]; then
    grep -F $check2 id_root.txt | awk '{print "File "$2" Changed";print "Executing Copy";system("cp "$2" ~/dotfiles/ ")}'
  fi
  done < dot_dir.txt 3< root_dir.txt
}

dot_files
read_comp
