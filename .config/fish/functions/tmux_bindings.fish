# ───── Key Bindings (Vi mode + TMUX fixes) ─────────────────────
function fish_user_key_bindings
    # Enable vi key bindings globally
    fish_vi_key_bindings

    # Only apply these fixes when inside tmux
    if set -q TMUX
        bind \e[1~ beginning-of-line      # Home
        bind \e[4~ end-of-line            # End
        bind \e[A  history-search-backward
        bind \e[B  history-search-forward
    end

    # These work everywhere (tmux or not)
    bind \cl clear-screen                 # Ctrl+l
    bind \cp history-search-backward      # Ctrl+p
    bind \cn history-search-forward       # Ctrl+n
    bind \cx backward-kill-word           # Ctrl+x
    bind \cy insert-last-argument         # Ctrl+y (like Bash !$)
end
