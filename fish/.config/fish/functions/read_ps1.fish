function read_ps1
    set -l ps1 '\h\[\e[1;34m\]\w\[\e[m\] \[\e[1;32m\]\$\[\e[m\] '
    set -l special
    set -l str
    for c in (string split '' -- $argv)
        if set -q special[1]
            switch $c
                case h
                    set -a str $hostname # (date)
                case '[' ']'
                    # ignore, irrelevant to us
                case e
                    set -a str \e
                case w
                    set -a str (prompt_pwd -D0 -d0)
                case '$'
                    fish_is_root_user
                    and set -a str '#'
                    or set -a str '$'
                case '\\'
                    set -e special[1]
                case '*'
                    echo "FAILURE: Got escaped '$c'" >&2
            end
            set -e special[1]
        else
            if test "$c" = '\\'
                set special 1
            else
                set -a str $c
            end
        end
    end

    string join '' -- $str
end
