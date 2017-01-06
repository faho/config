function psgrep
	set -l ret 1
    for i in $argv
	    if set -l p (pgrep -f $i)
            set ret 0
	        ps up $p
        end
    end
    return $ret
end
