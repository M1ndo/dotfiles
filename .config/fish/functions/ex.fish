function ex --description "Extract many types of archives" --argument-names file
    if test -f $file
        switch $file
            case '*.tar.bz2' '*.tbz2'
                tar xjf $file

            case '*.tar.gz' '*.tgz'
                tar xzf $file

            case '*.tar.xz'
                tar xJf $file   # uppercase J for xz

            case '*.tar.zst' '*.tzst'
                tar --use-compress-program=unzstd -xf $file
                # or just: tar xf $file (modern tar auto-detects zstd)

            case '*.bz2'
                bunzip2 $file

            case '*.rar'
                unrar x $file

            case '*.gz'
                gunzip $file

            case '*.tar'
                tar xf $file

            case '*.zip' '*.ZIP'
                unzip $file

            case '*.Z'
                uncompress $file

            case '*.7z' '*.7zip'
                7z x $file

            case '*'
                echo "'$file' cannot be extracted by ex()"
                return 1
        end
    else
        echo "'$file' is not a valid file"
        return 1
    end
end

