function mdtable
    argparse d/delimiter= -- $argv
    or return

    isatty stdin
    and return

    set -ql _flag_d[1]
    and set _flag_d -d $_flag_d[-1]

    set -q argv[1]
    or read -la $_flag_d argv

    string join '|' -- "" $argv ""
    set -l nums (count $argv)
    string join '|' -- "" (string repeat -n $nums :---\n)
    
    while read $_flag_d -la line
        set -q line[$nums]
        or set line[$nums] ""

        test (count $line) -gt $nums
        and set -l new (string join -- "$_flag_d[-1]" $line[$nums..])
        and set -e line[$nums..]
        and set line[$nums] $new

        string join '|' -- "" $line ""
    end
end
