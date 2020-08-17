# Defined in /tmp/fish.g5mHoi/histexpand.fish @ line 2
function histexpand
    set -l token (commandline -ct)
    echo "Token is $token"
    switch "$token"
        case '!!'
            commandline -rt $history[1]
        case '!$'
            commandline -t ""
            commandline -f history-token-search-backward
        case '*'
            if string match -qr '\^.*\^.*' -- $token
                echo "^^" >&2
                set re (string match -ra '\^[^^]*' -- $token)
                commandline -i -- (string replace -- $re $history[1])
            else
                commandline -i -- "$argv"
            end
    end
end
