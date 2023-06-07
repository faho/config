function fish_confirm
    argparse m/mode= l/long -- $argv
    or return

    set -l mode repeat
    if set -ql _flag_m[1]
        if contains -- $_flag_m[-1] repeat assume-{yes,no}
            set mode $_flag_m[-1]
        else
            echo "No known mode: $_flag_m[-1]" >&2
            return 2
        end
    end

    set -l ack y
    set -l deny n

    if test $mode = assume-yes
        set -ql _flag_l
        and set ack Yes
        and set deny no
        or set ack Y
    else if test $mode = assume-no
        set -ql _flag_l
        and set deny No
        and set ack yes
        or set deny N
    else if set -ql _flag_l
        set ack yes
        set deny no
    end

    set -l prompt "$argv ($ack/$deny) "

    while true
        read --prompt-str "$prompt" -l response
        or begin
            if test $mode = assume-yes
                return 0
            else if test $mode = assume-no
                return 1
            else
                continue
            end
        end

        if string match -qir "^$ack\$" -- $response
            return 0
        end
            
        if string match -qir "^$deny\$" -- $response
            return 1
        end

        if test $mode = assume-yes
            return 0
        else if test $mode = assume-no
            return 1
        end
    end
end
