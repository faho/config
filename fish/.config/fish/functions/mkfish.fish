function mkfish
	set -l branch master
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
	sed -e "s|source=.*|source=('git+file:///home/alfa/dev/fish-shell#branch=$branch')|" -i PKGBUILD
	makepkg -sir $opts
end
