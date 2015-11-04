function psgrep
	[ -z $argv ]; and return
	set -l p (pgrep -f $argv)
	or return 1
	ps up $p
end
