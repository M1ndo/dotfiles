# ~/.config/fish/functions/genpdf2.fish
function genpdf2 --description "Robust LaTeX compile with latexmk (cleans aux files)"
    set -l fileTex $argv[1]
    if not test -f "$fileTex"
        echo "genpdf2: file not found: $fileTex"
        return 1
    end

    set -l fileOut (basename $fileTex .tex).pdf
    set -l dir (mktemp -d)

    latexmk -f -pdf -shell-escape -interaction=nonstopmode \
            --output-directory="$dir" "$fileTex"

    if test -f "$dir/$fileOut"
        mv "$dir/$fileOut" .
    end

    # Clean up auxiliary files in current directory
    rm -f *.aux *.log *.out *.toc *.fls *.fdb_latexmk *.synctex.gz 2>/dev/null

    rm -rf $dir
end
