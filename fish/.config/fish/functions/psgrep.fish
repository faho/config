function psgrep
	[ -z $argv ]; and return
ps aux | grep $argv
end
