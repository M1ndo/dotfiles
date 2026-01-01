# ~/.config/fish/functions/genpdf.fish
function genpdf --description "Quick compile LaTeX → PDF (lualatex)"
    set -l file $argv[1]
    if not test -f "$file"
        echo "genpdf: file not found: $file"
        return 1
    end

    set -l dir (mktemp -d)
    lualatex -shell-escape -interaction nonstopmode --output-directory="$dir" "$file"
    mv $dir/*.pdf . 2>/dev/null
    rm -rf $dir
end
