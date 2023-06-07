function humanize
    set -l prefixes "" K M G T E
    set -l num 1
    argparse d/delimiter= n/num= -- $argv
    or return
    set -ql _flag_d
    and set _flag_d -d $_flag_d
    set -ql _flag_n
    and set num $_flag_n

    set -l args $argv

    while set -q args[1]
        or not isatty stdin && read -la $_flag_d args
        
        set -l pre (math -s0 "min log10($args[$num]) / 3 + 1, $(count $prefixes)" 2>/dev/null; or echo 1)
        set -l ch $prefixes[$pre]

        set args[$num] (math -s3 -- "$args[$num] / (1000 ^ ($pre - 1))")$ch
        if set -q _flag_d[2]
            string join $_flag_d[2] -- $args
        else
            string join " " -- $args
        end

        set -e argv[1]
        set args $argv
    end
end
