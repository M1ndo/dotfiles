# =============================================
# ~/.config/fish/config.fish
# Full conversion of your .bashrc → Fish shell
# =============================================

# ───── PATH ─────────────────────────────────────
fish_add_path -m \
    ~/.emacs.d/bin \
    ~/.local/bin \
    ~/.local/share/gem/ruby/3.4.0/bin \
    ~/go/bin \
    /usr/bin/vendor_perl \
    /opt/metasploit/tools/exploit

# ───── Environment Variables ─────────────────────
set -gx EDITOR "emacsclient -r"
set -gx TERM "xterm-256color"
set -gx GO111MODULE on
set -gx MANPAGER "sh -c 'sed -e s/.\\\\x08//g | bat -l man -p'"

# Optional: nicer pager
# set -gx PAGER "bat"

# ───── Keychain (SSH/GPG agent) ─────────────────
if type -q keychain
    keychain --eval --quiet --noask --systemd | source
end

# ───── History Settings ─────────────────────────
set -gx HISTSIZE 10000
set -gx SAVEHIST 50000
# Fish already appends history by default → no histappend needed

# ───── Shell Options (Fish equivalents) ─────────
set -g fish_autosuggestion_enabled 1        # like autocd
set -g fish_complete_cd 1             # cdspell-like behavior
set -g fish_checkwinsize yes          # checkwinsize


# ───── Aliases ─────────────────────────────────
# Navigation
alias ..    "cd .."
alias ...   "cd ../.."
alias doc   "cd ~/Documents"
alias dow   "cd ~/Downloads"

# Emacs
alias ope   "emacsclient -r"
alias emax  "emacsclient -r --tty"

# Trash instead of rm
alias rm    "trash"
alias rms   "/bin/rm"
alias del   "trash"

# Sudo → doas
alias sudo  "doas"

# Clear with full reset
alias clear "echo -e '\x1b[2J\x1b[1;1H'"

# ls → exa (or lsd if you prefer)
alias ls    "exa -gl --group-directories-first"
alias ll    "exa -gal --group-directories-first"
alias la    "exa -ga --group-directories-first"
alias lt    "exa -glT --group-directories-first"
alias l     "ls"

# Grep with color
alias grep  "grep --color=auto"
alias egrep "egrep --color=auto"
alias fgrep "fgrep --color=auto"

# Pacman / Yay shortcuts
alias src   "yay -Ss"
alias ins   "yay -S"
alias up    "yay -Sy"
alias upd   "yay -Syu"
alias yrms  "yay -R"
alias pcsrc "pacman -Ss"
alias pcin  "sudo pacman -S"
alias pcr   "sudo pacman -R"
alias pcrn  "sudo pacman -Rscu"
alias sy    "sudo pacman -Sy"
alias syu   "sudo pacman -Syu"
alias syy   "sudo pacman -Syy"

# Git & misc
alias gic   "git clone"
alias pg  "ping"
alias ips "curl -s ifconfig.co"

# Downloaders
alias dwn "aria2c --max-connection-per-server=16 --enable-dht=true --split=8 --min-split-size=10M --max-concurrent-downloads=4 --max-overall-download-limit=0 --retry-wait=5 --file-allocation=falloc"

# Broot
alias br "br -dhp"
alias bs "br --sizes"

# PDF & LaTeX
alias zt "zathura"
alias tlmgr "/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode"

# Mpv YouTube quality
alias plhd  "mpv --ytdl-format='bestvideo[height<=?720]+bestaudio/best'"
alias plhd+ "mpv --ytdl-format='bestvideo[height<=?1080]+bestaudio/best'"

# Screen resolution hacks
alias revp  "xrandr --output LVDS1 --mode 1366x768 --panning 1920x1080 --scale 1.40556369x1.40625"
alias revpo "xrandr --output LVDS1 --mode 1366x768 --panning 1366x768 --scale 1x1"

# Software rendering (laptop fix)
alias scv "env LIBGL_ALWAYS_SOFTWARE=1"

# Copy/paste
alias cp   "cp -i"
alias rcpp "rsync -hvrPt"
alias df   "df -h"
alias du   "du -h"
alias free "free -m"
alias vifm "./.config/vifm/scripts/vifmrun"
alias xc   "xclip -sel clip -rmlastnl"
alias xcc  "xclip -sel prim -rmlastnl"

# System tools
alias microcode "grep . /sys/devices/system/cpu/vulnerabilities/*"
alias mirror    "sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias jctl      "journalctl -p 3 -xb"
alias sysu      "systemctl --user status"
alias sysr      "systemctl status"
alias tb        "nc termbin.com 9999"

# Xresources reload
alias xd "xrdb ~/.Xresources"

# Lock screen
alias bls "betterlockscreen -l --display 1"

# Recent packages
alias rip      "expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong  "expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

# ───── Directory shortcuts ───────────────────────
set -gx WEBROOT "~/Templates/Website"
alias web     "cd $WEBROOT/WebRoot"
alias webpush "cd $WEBROOT/Push_Web"
alias webms   "cd $WEBROOT/Music_Player/My_Music"
alias webmsp  "cd $WEBROOT/My_Music"

# DarkOs repos
alias darksrc "cd ~/DarkOs/Full/Builds/repo_src"
alias darkpkg "cd ~/DarkOs/Full/Builds/pkg_builds"
alias darkrep "cd ~/DarkOs/Full/DarkOs-Repo"
alias darkope "cd ~/DarkOs/Full/Operating_System"
set -gx DARKREP "/home/llove/DarkOs/Full/DarkOs-Repo/x86_64"

# ───── Startup commands ───────────────────────────
# Random color script (optional)
# ~/.bin/shuffle.py

# 16 million color test (optional, comment out if annoying)
# 16s

# Fancy prompt (Starship is the best option in Fish)
#if type -q starship
#    starship init fish | source
#end

# Better cd with zoxide (replaces old cd behavior)
if type -q zoxide
    zoxide init fish | source
end

# TheFuck (instant command correction)
if type -q thefuck
    thefuck --alias | source
end
