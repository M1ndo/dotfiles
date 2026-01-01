# ~/.config/fish/functions/gradient_cols.fish
function gradient_cols --description "Random gradient preset"
    set -l preset (shuf ~/.bin/preset_list | head -n1)
    gradient -p $preset -H1 -W75
end
