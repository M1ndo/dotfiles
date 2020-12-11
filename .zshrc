# Maintainer Ybenel (r2dr0dn@pm.me)

export ZSH=/home/ybenel/.oh-my-zsh
ZSH_THEME="miloshadzic"
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )
# CASE_SENSITIVE="true"
# HYPHEN_INSENSITIVE="true"
# DISABLE_AUTO_UPDATE="true"
# DISABLE_UPDATE_PROMPT="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_MAGIC_FUNCTIONS=true
# DISABLE_LS_COLORS="true"
# DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# HIST_STAMPS="mm/dd/yyyy"
plugins=("git" "thefuck")
source $ZSH/oh-my-zsh.sh

# Man Page
export MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\""
# export MANPATH="/usr/local/man:$MANPATH"

export EDITOR='nvim'


source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

setopt GLOB_DOTS

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups
#if [ -d "$HOME/.bin" ] ;
#  then PATH="$HOME/.bin:$PATH"
#fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

xhost +local:root > /dev/null 2>&1

complete -cf sudo
setopt autocd
# Enable history appending instead of overwriting.  #139609
setopt histappend
setopt histignoredups

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
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

### ALIASES ###

# root privileges
#alias doas="doas --"

# navigation
alias ..='cd ..'
alias ...='cd ../..'
# vim
alias vim=nvim

# broot
alias br='br -dhp'
alias bs='br --sizes'

#Source .bashrc
alias sr='source ~/.bashrc'
alias zrc='source ~/.zshrc'

# Changing "ls" to "lsd"

alias ll='lsd -al' # my preferred listing
alias la='lsd -a'  # all files and dirs
alias ls='lsd -l'  # long format
alias lt='lsd -a --tree' # tree listing
alias l="ls"

# yay shortcuts
alias src='yay -Ss'
alias ins='yay -S'
alias up='yay -Sy'
alias upd='yay -Syu'
alias rms='yay -R'
alias pcsrc='pacman -Ss'
alias pcin='sudo pacman -S'
alias pcr='sudo pacman -R'
alias sy='sudo pacman -Sy'
alias syu='sudo pacman -Syu'
alias syy='sudo pacman -Syy'
# Saving Time Typing
alias gic='git clone'
alias pg='ping'   # Extra

# Overwrite .Xresources To take effect of the new settings
alias xd='xrdb ~/.Xresources'

# adding flags
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias du='du -h'
alias free='free -m'                      # show sizes in MB
alias lynx='lynx -cfg=~/.lynx/lynx.cfg -lss=~/.lynx/lynx.lss -vikeys'
alias vifm='./.config/vifm/scripts/vifmrun'

# Aliases
alias doc='cd ~/Documents'
alias dow='cd ~/Downloads'
alias ips="curl -s ifconfig.co | grep 'IP</span>:'| cut -d '<' -f 4 | sed 's/\/span>://'"

# Hacking  Tools Shortcuts
alias searchsploit='/opt/exploitdb/searchsploit'
alias conx="dow && cd conx && sudo openvpn ybenel.ovpn"
alias conx2="dow && cd conx && sudo openvpn Thm.ovpn"
alias htb='doc && cd Sec/htb/'
alias thm='doc && cd Sec/thm'

# Awesome Screen Locker
alias bls="betterlockscreen -u ~/Pictures/WallOne/Mandalorian4.jpg -l -t 'This Is The Way'"

#check vulnerabilities microcode
alias microcode='grep . /sys/devices/system/cpu/vulnerabilities/*'

#get fastest mirrors in your neighborhood
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"

#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# the terminal rickroll
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

# bare git repo alias for dotfiles
#alias config="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"

# termbin
alias tb="nc termbin.com 9999"

### SET VI MODE IN BASH SHELL
set -o vi

#Recent Installed Packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

### RANDOM COLOR SCRIPT ###
$HOME/.bin/shuffle.py
