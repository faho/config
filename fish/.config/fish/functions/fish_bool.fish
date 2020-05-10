function fish_bool
    for a in $argv
        if string match -qir '1|y(es)?|t(rue)?|on' -- $a
            return 0
        end
    end
    return 1
end
