# ~/.config/fish/functions/vterm_cmd.fish
function vterm_cmd
    set -l vterm_elisp ""
    for arg in $argv
        set vterm_elisp "$vterm_elisp\""(echo $arg | sed 's/\\/\\\\/g; s/"/\\"/g')" "
    end
    vterm_printf "51;E$vterm_elisp"
end
