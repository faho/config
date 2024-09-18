function fish_confirm
    argparse m/mode= l/long -- $argv
    or return

    if not isatty stdin
        if test $mode = assume-yes
            return 0
        else if test $mode = assume-no
            return 1
        end
    end

    set -q argv[1]
    or set argv (_ "Confirm?")

    set -l mode repeat
    if set -ql _flag_m[1]
        if contains -- $_flag_m[-1] repeat assume-{yes,no}
            set mode $_flag_m[-1]
        else
            printf (_ "No known mode: %s") "$_flag_m[-1]" >&2
            return 2
        end
    end

    set -l ack
    set -l deny

    if test $mode = assume-yes
        set -ql _flag_l
        and set ack (_ Yes)
        and set deny (_ no)
        or set ack (_ Y)
    else if test $mode = assume-no
        set -ql _flag_l
        and set deny (_ No)
        and set ack (_ yes)
        or set deny (_ N)
    else if set -ql _flag_l
        set ack (_ yes)
        set deny (_ no)
    else
        set ack (_ y)
        set deny (_ n)
    end

    set -l prompt "$argv [$ack/$deny] "

    set ack (string escape --style=regex -- $ack)
    set deny (string escape --style=regex -- $deny)

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
