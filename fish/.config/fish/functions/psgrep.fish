function psgrep
	set -l ret 1
    for i in $argv
	    if pgrep -af $i
            set ret 0
        end
    end
    return $ret
end
