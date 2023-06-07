# Defined in /tmp/fish.jEozma/is.fish @ line 2
function is
    if not set -q argv[1]
        echo AAAAAAARGRGGRGR >&2
        return 1
    end
    set -l cmd $argv[1]
    set -e argv[1]
    switch $cmd
        case empty
            for f in $argv
                if test -n $f
                    return 1
                end
            end
            return 0
        case notempty
            for f in $argv
                if test -n $f
                    return 0
                end
            end
            return 1
        case ascending
            set -l num
            for f in $argv
                if not set -q num[1]
                    set num $f
                end
                if test $f -lt $num
                    return 1
                end
            end
            return 1
        case '*'
            echo NBBBBBBB >&2
            return 1
    end
    return 1
end
