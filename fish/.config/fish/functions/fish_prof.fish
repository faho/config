function fish_prof
    # TODO: min/max level?
    argparse -x p,m p/percentile= m/minimum= c/children -- $argv
    or return

    set -l proffile $argv[1]
    if not path is -rf -- $proffile
        echo "Not a readable file: $proffile" >&2
        return 1
    end

    set -l min $_flag_m 3000
    if set -ql _flag_p[1]
        set -l nums (string split \t -f 2 < $proffile | path sort)[..-2]
        set -l perc (math -s0 (count $nums) x "($_flag_p[1] / 100)")
        set min $nums[$perc]
        if not set -q min[1]
            echo "Invalid percentile"
            return 1
        end
    end

    begin
        set -l curdep
        read -l x
        while read -l -d \t single sum dep command
            if set -q curdep[1]
                if set -ql _flag_children[1]; and test "$(string length -- $dep)" -gt "$(string length -- $curdep)"
                    echo $single $sum $dep $command
                else
                    set curdep
                end
            else if test $sum -gt $min[1]
                echo $single $sum $dep $command
                set curdep $dep
            end
        end
    end < $proffile
    true
end
