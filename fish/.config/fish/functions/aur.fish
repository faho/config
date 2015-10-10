function aur --description 'Quite possibly the stupidest aur helper ever invented'
	# TODO: This does not handle dependencies - they don't seem to be exported via the json interface
	# TODO: This does not correctly handle searches with multiple arguments
	# TODO: Unhardcode ~/dev/build/{current,new}
	# TODO: Implement "demote"
	# TODO: Implement "build"
	# There seems to be no way to do this
	switch $argv[1]
		case "search" "clone" "promote" "demote" "rm"
			set mode $argv[1]
			set -e argv[1]
		case "*"
			set mode search
	end
			
	if not set -q argv[1]
		echo "More arguments needed, dumbass"
		return 1
	end

	if contains -- $mode "search" "clone"
		set -l tmp (curl -G --data-urlencode "arg=$argv" "https://aur.archlinux.org/rpc.php?type=search" -s)
		set -l names (echo $tmp | jshon -e results -a -e Name -u)
		set -l urls "https://aur.archlinux.org/"(echo $tmp | jshon -e results -a -e PackageBase -u)".git"
		set -l descs (echo $tmp | jshon -e results -a -e Description -u)
		if [ $mode = search ]
			for i in (seq (count $names))
				printf "%-40s: %-60s (%s)\n" (set_color blue)$names[$i](set_color normal) (set_color normal)$urls[$i] (set_color yellow)$descs[$i](set_color normal)
			end
		else if [ $mode = clone ]
			set -l clonenum 1
			if set -q names[2]
				for i in (seq (count $names))
					printf "%d - %-40s: %s\n" $i (set_color blue)$names[$i] (set_color yellow)$descs[$i](set_color normal)
				end
				echo "Which one would you like to clone, master?"
				read clonenum
			end
			# Allow clone ranges
			set clonenum (string split ".." -- $clonenum)
			if set -q clonenum[2]
				set clonenum (seq $clonenum[1] $clonenum[2])
			end
			for i in $clonenum
				git clone $urls[$i] ~/dev/build/new/$names[$i]
			end
		end
		return 0
	else if [ $mode = "rm" ]
		rm -rf ~/dev/build/new/$argv
	else if [ $mode = "promote" ]
		mv ~/dev/build/new/$argv ~/dev/build/current
		for pkg in $argv
			git -C ~/dev/build/current submodule add ./$pkg
		end
	else if [ $mode = "demote" ]
		echo "Demote mode is not implemented yet"
	end
end
