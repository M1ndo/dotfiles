# ~/.config/fish/functions/open.fish
function open --description "Open file or directory in Emacs (vterm integration)"
    set -l path (realpath $argv[1] 2>/dev/null; or pwd)
    vterm_cmd find-file $path
end
