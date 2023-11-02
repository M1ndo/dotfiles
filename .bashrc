# Maintainer Ybenel (ybenel@molero.xyz)
# Ble.sh
[[ $- == *i* ]] && source /usr/share/blesh/ble.sh --noattach

# Including Local Bin Tool In The Path
PATH="$HOME/.emacs.d/bin:$HOME/.local/bin:$HOME/.gem/ruby/3.0.0/bin:$PATH:$HOME/go/bin"
# Setting Editor As Emacs
EDITOR="emacsclient -r"

# Source Skey
# source $HOME/.skey
source $HOME/.shortcuts

# Exporting Term Colors To Xterm 
# Note: For Better Colors Change To "st-256color" if you have st installed
export TERM="xterm-256color"
export GO111MODULE=on

# TMux fix
if [[ -n "$TMUX" ]]; then
    bind '"\e[1~":"\eOH"'
    bind '"\e[4~":"\eOF"'
    bind '"\e[A":history-search-backward'
    bind '"\e[B":history-search-forward'
    bind '"\C-l":clear-screen'
    bind '"\C-p":history-search-backward'
    bind '"\C-n":history-search-forward'
    bind '"\C-x":backward-kill-word'
    bind '"\C-y":insert-last-argument'
fi


# Maximize History
HISTSIZE=10000
HISTFILESIZE=50000
[[ $- != *i* ]] && return

# A List Of Colors 
# To Run Type: colors
colors() {
	local fgc bgc vals seq0

	printf "Color escapes are %s\n" '\e[${value};...;${value}m'
	printf "Values 30..37 are \e[33mforeground colors\e[m\n"
	printf "Values 40..47 are \e[43mbackground colors\e[m\n"
	printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

	# foreground colors
	for fgc in {30..37}; do
		# background colors
		for bgc in {40..47}; do
			fgc=${fgc#37} # white
			bgc=${bgc#40} # black

			vals="${fgc:+$fgc;}${bgc}"
			vals=${vals%%;}

			seq0="${vals:+\e[${vals}m}"
			printf "  %-9s" "${seq0:-(default)}"
			printf " ${seq0}TEXT\e[m"
			printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
		done
		echo; echo
	done
}

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Change the window title of X terminals
case ${TERM} in
	xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
		;;
	screen*)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
		;;
esac

use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
	&& type -P dircolors >/dev/null \
	&& match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
	# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
	if type -P dircolors >/dev/null ; then
		if [[ -f ~/.dir_colors ]] ; then
			eval $(dircolors -b ~/.dir_colors)
		elif [[ -f /etc/DIR_COLORS ]] ; then
			eval $(dircolors -b /etc/DIR_COLORS)
		fi
	fi

	if [[ ${EUID} == 0 ]] ; then
		PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
	else
		PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
	fi

		grep='grep --colour=auto' \
		egrep='egrep --colour=auto' \
		fgrep='fgrep --colour=auto'
else
	if [[ ${EUID} == 0 ]] ; then
		# show root@ when we don't have colors
		PS1='\u@\h \W \$ '
	else
		PS1='\u@\h \w \$ '
	fi
fi

unset use_color safe_term match_lhs sh

xhost +local:root > /dev/null 2>&1

complete -cf sudo
complete -cf doas

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s autocd
shopt -s cdspell
# export QT_SELECT=4

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.tar.zst)        tar xf $1      ;;
      *.tar.xz)        tar xf $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Get Ip Addresses
function ipc () {
  curl ipinfo.io/$1
}

# Display True 16 million colors 
function 16s() {
    awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";}'
}

function gradient_cols () {
    preset=$(shuffle ~/.bin/preset_list | head -1)
    gradient -p $preset -H1 -W75
}

## Open file in dired
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

open() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

say() {
    vterm_cmd message "%s" "$*"
}


genpdf() {

    DIR=$(mktemp -d)
    lualatex -shell-escape -interaction nonstopmode --output-directory="$DIR" "$1"

    mv "$DIR/"*pdf .

}

genpdf2() {

    DIR=$(mktemp -d)
    latexmk -f -pdf -lualatex -shell-escape -interaction=nonstopmode --output-directory="$DIR" "$1"
    mv "$DIR/"*pdf .

}

#calc ()
#{
#  (( d = $1 ))
#  echo $d
#}

### ALIASES ###
# Open Files
alias ope='emacsclient -r '

# navigation
alias ..='cd ..' 
alias ...='cd ../..'

# vim

# Trash
alias rm='trash'
alias rms='/bin/rm'

# Clear
alias clear='echo -en "\x1b[2J\x1b[1;1H"'

# Doas as Sudo
alias sudo='doas'

# broot
alias br='br -dhp'
alias bs='br --sizes'

# Aria2c 
alias dwn='aria2c --max-connection-per-server=16 --split=8 --min-split-size=10M --max-concurrent-downloads=4 --max-overall-download-limit=0 --retry-wait=5 --file-allocation=prealloc'

#Source .bashrc
alias sr='source ~/.bashrc'
alias zrc='source ~/.zshrc'

# Add Grep Colors
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Changing "ls" to "lsd"
# alias ll='lsd -al --icon never' # my preferred listing
# alias la='lsd -a --icon never'  # all files and dirs
# alias ls='lsd -l --icon never'  # long format
# alias lt='lsd -a --tree --icon never' # tree listing
alias ll='exa -gal  --group-directories-first' # my preferred listing
alias la='exa -ga --group-directories-first'  # all files and dirs
alias ls='exa -gl --group-directories-first'  # long format
alias lt='exa -glT  --group-directories-first' # tree listing
alias l="ls"

# yay shortcuts
alias src='yay -Ss'
alias ins='yay -S'
alias up='yay -Sy'
alias upd='yay -Syu'
alias yrms='yay -R'
alias pcsrc='pacman -Ss'
alias pcin='sudo pacman -S'
alias pcr='sudo pacman -R'
alias pcrn='sudo pacman -Rscu'
alias sy='sudo pacman -Sy'
alias syu='sudo pacman -Syu'
alias syy='sudo pacman -Syy'
# Saving Time Typing 
alias gic='git clone' 
alias pg='ping'   # Extra

# Overwrite .Xresources To take effect of the new settings
alias xd='xrdb ~/.Xresources'

# Website Aliases
export WEBROOT="~/Templates/Website"
alias web="cd $WEBROOT/WebRoot"
alias webpush="cd $WEBROOT/Push_Web"
alias webms="cd $WEBROOT/Music_Player/My_Music"
alias webmsp="cd $WEBROOT/My_Music"

# DarkOs Aliases
alias darksrc="cd ~/DarkOs/Full/Builds/repo_src"
alias darkpkg="cd ~/DarkOs/Full/Builds/pkg_builds"
alias darkrep="cd ~/DarkOS/Full/DarkOs-Repo/"
alias darkope="cd ~/DarkOS/Full/Operating_System/"
export DARKREP="/home/llove/DarkOs/Full/DarkOs-Repo/x86_64/"

# Tlmgr
alias tlmgr='/usr/share/texmf-dist/scripts/texlive/tlmgr.pl  --usermode'

# Emacs
alias emax='emacsclient -r --tty'

# Pdf open
alias zt='zathura'


# Mpv Alias
alias plhd="mpv --ytdl-format='bestvideo[height<=?720]+bestaudio/best'"
alias plhd+="mpv --ytdl-format='bestvideo[height<=?1080]+bestaudio/best'"

# Res
alias revp="xrandr --output LVDS1 --mode 1366x768 --panning 1920x1080 --scale 1.40556369x1.40625"
alias revpo="xrandr --output LVDS1 --mode 1366x768 --panning 1366x768 --scale 1x1"

# Add A Special Env For My Laptop
alias scv='LIBGL_ALWAYS_SOFTWARE=1'

# adding flags
alias cp="cp -i"                          # confirm before overwriting something
alias rcpp="rsync -hvrPt" ### Rsync Copy (New And Modified files/directories)
alias df='df -h'                          # human-readable sizes
alias du='du -h'
alias free='free -m'                      # show sizes in MB
alias lynx='lynx -cfg=~/.lynx/lynx.cfg -lss=~/.lynx/lynx.lss -vikeys'
alias vifm='./.config/vifm/scripts/vifmrun'

# More Aliases 
alias doc='cd ~/Documents'
alias dow='cd ~/Downloads'

#alias ips="curl -s ifconfig.co | grep 'IP</span>:'| cut -d '<' -f 4 | sed 's/\/span>://'"
alias ips="curl -s ifconfig.co" 

# Awesome Screen Locker
#alias bls="betterlockscreen -w ~/Pictures/Backgrounds/Carina_Nebula.jpg -l -t 'Victory Is Mine !'"
alias bls="betterlockscreen -l --display 1"  

# xclip 
alias xc='xclip -sel clip -rmlastnl'
alias xcc='xclip -sel prim -rmlastnl'

#check vulnerabilities microcode
alias microcode='grep . /sys/devices/system/cpu/vulnerabilities/*'

#get fastest mirrors in your neighborhood
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"

#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"
alias sysu="systemctl --user status "
alias sysr="systemctl status "

# termbin
alias tb="nc termbin.com 9999"

### SET VI MODE IN BASH SHELL
set -o vi

### SET VIM/NVIM AS MANPAGER ###
#export MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\""
export MANPAGER="nvim +Man!"
#export MANPAGER="sh -c 'sed -e s/.\\\\x08//g | bat -l man -p'"
#export MANPAGER="nvimpager"
#export PAGER="nvimpager"

#Recent Installed Packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

### BASH INSULTER ###
if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi

### RANDOM COLOR SCRIPT ###
#$HOME/.bin/shuffle.py

# PS1 Customization "~$ "
export PS1="\[\e[1;49;32m\]\W \[\e[m\]\[\e[1;49;96m\]\\$\[\e[1;49;39m\] "
#16s
#gradient_cols
#export PS1='\[\e[0m\]\[\e[48;5;236m\]\[\e[38;5;105m\]\u\[\e[38;5;105m\]@\[\e[38;5;105m\]\h\[\e[38;5;105m\] \[\e[38;5;221m\]\w\[\e[38;5;221m\]\[\e[38;5;105m\]\[\e[0m\]\[\e[38;5;236m\]\342\226\214\342\226\214\342\226\214\[\e[0m\]'
#export PS1='\[\e[31;1;48;234m\]\u \[\e[38;5;240m\]on \[\e[1;38;5;28;48;234m\]\h \[\e[38;5;54m\]\d \@\[\e[0m\]\n\[\e[38;5;105m\][\W] \[\e[1m\]\$\e[0m\] '
eval $(thefuck --alias)
eval "$(zoxide init bash)"
eval "$(starship init bash)"
[[ ${BLE_VERSION-} ]] && ble-attach
