# Defined in /tmp/fish.QAafTh/expand_path.fish @ line 2
function expand_path --description 'Expand arguments to be used as paths'
    echo "ARGV IS $argv"
    for p in $argv
        # TODO: Expand variables as well
        # Expand "~".
        if string match -q '~*' -- $p
            set -l user (string replace -r '~([^/]*)/.*$' '$1' -- $p)
            echo "User is $user"
            # Tilde-expansion happens after variable expansion.
            if test -n "$user"
                set p (string replace -r '~[^/]*/' ~$user/ -- $p)
            else
                set p (string replace -r '~[^/]*/' ~/ -- $p)
            end
        end
        echo $p
    end
end
