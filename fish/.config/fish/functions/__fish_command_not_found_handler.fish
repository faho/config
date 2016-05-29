function __fish_command_not_found_handler --on-event fish_command_not_found
	set -l paths $argv[1]
	# If we've not been given an absolute path, try $PATH as the starting point
	string match -q '/*' -- $argv[1]; or set paths $PATH/$argv[1]
	set -l packages
	set -l __packages
	# The `tr` is ugly, but `string split` doesn't seem to like the \0
	if set -l data (pacman -Fo --machinereadable $paths | tr "\0" \n)
		while set -q data[4]
			echo $data[1..4] | read -l repo name ver path
			set packages $packages $name
			set __packages $__packages "$repo/"(set_color brblue)"$name "(set_color brgrey)"$ver "(set_color normal)"("(string trim -- /$path)")"
			set -e data[1..4]
		end
	end
	# pkgfile --binaries --verbose -- $argv[1] ^/dev/null | while read name ver path
	# 	set packages $packages $name
	# 	set __packages $__packages (set_color brblue)"$name "(set_color brgrey)"$ver "(set_color normal)"("(string trim -- $path)")"
	# end
	if set -q __packages[1]
		printf "%s may be found in the following packages:\n" "$argv[1]"
		for i in (seq (count $__packages))
			printf (set_color white)"%d:  %s\n" $i $__packages[$i]
		end
		read -l num -p "echo (set_color white)'Install package (num)? '(set_color normal)"
		if test $num -gt 0
			sudo pacman -S $packages[$num]
			if test $status -eq 0
				set -l escapedargs (string escape -n -- $argv)
				read -l yn -p "echo 'Run \"$escapedargs\"(y/n)? '"
				if test $yn = "y" -o $yn = "yes"
					eval $escapedargs
				end
			end
			return 0
		end
	else
		__fish_default_command_not_found_handler $argv[1]
	end
end
