# ~/.config/fish/functions/ipc.fish
function ipc --description "Get IP info from ipinfo.io"
    if test (count $argv) -eq 0
        curl -s ipinfo.io
    else
        curl -s ipinfo.io/$argv[1]
    end
end
