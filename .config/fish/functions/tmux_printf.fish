# ~/.config/fish/functions/vterm_printf.fish
function vterm_printf
    if set -q TMUX; and string match -q 'tmux*' $TERM; or string match -q 'screen*' $TERM
        # tmux
        printf '\ePtmux;\e\e]%s\007\e\\' "$argv[1]"
    else if string match -q 'screen*' $TERM
        # screen
        printf '\eP\e]%s\007\e\\' "$argv[1]"
    else
        printf '\e]%s\e\\' "$argv[1]"
    end
end
