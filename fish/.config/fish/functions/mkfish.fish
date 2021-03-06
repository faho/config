function mkfish --wraps makepkg
	set -lx branch master
	set -l opts
	for arg in $argv
		switch $arg
			case "-*"
				set opts $opts $arg
			case "*"
				set branch $arg
		end
	end
	cd ~/dev/build/fish-shell-git
    set branch "#branch=$branch"
	makepkg -sir $opts
end
