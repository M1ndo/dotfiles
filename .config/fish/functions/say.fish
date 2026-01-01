# ~/.config/fish/functions/say.fish
function say --description "Send message to Emacs minibuffer from vterm"
    vterm_cmd message "%s" "$argv"
end
