function aur --description 'Quite possibly the stupidest aur helper ever invented'
	# TODO: This does not correctly handle searches with multiple arguments
	# TODO: Implement "demote" (move from current to new)
	# TODO: Implement "build" (makepkg, possibly with dependencies)
	set -q aurqueue; or set -l aurqueue ~/dev/build/new
	set -q aurpkgs; or set -q aurpkgs ~/dev/build/current
	set -l mode search
	set -l yellow ""
	set -l blue ""
	set -l normal ""
	if isatty stdout
		set yellow (set_color yellow)
		set normal (set_color normal)
		set blue (set_color blue)
	end

	switch $argv[1]
		case "search" "clone" "promote" "demote" "rm" "update" # TODO: "build"
			set mode $argv[1]
			set -e argv[1]
	end
			
	if not set -q argv[1]
		echo "More arguments needed, dumbass"
		return 1
	end

	if contains -- $mode "search" "clone"
		if [ $mode = search ]
			set -l arg
			for a in $argv
				set arg --data-urlencode "arg=$a" $arg
			end
			# This line dominates the profile, the jshon calls don't matter
			set -l tmp (curl -G $arg "https://aur.archlinux.org/rpc.php?v=4&type=search" -s)
			if [ (echo $tmp | jshon -e resultcount) -eq "0" ]
				echo "No results found"
				return 1
			end
			set -l names (echo $tmp | jshon -e results -a -e Name -u)
			set -l descs (echo $tmp | jshon -e results -a -e Description -u)
			set -l urls (echo $tmp | jshon -e results -a -e URL -u)
			set -l versions (echo $tmp | jshon -e results -a -e Version -u)
			for i in (seq (count $names))
				printf "%s v%s\n\t%s (%s)\n" $blue$names[$i]$normal $versions[$i] $descs[$i] $yellow$urls[$i]$normal
			end
		else if [ $mode = clone ]
			# For some reason only explicitly encoding the version gets us deps
			set -l tmp (curl -G --data-urlencode "arg=$argv" "https://aur.archlinux.org/rpc.php?v=4&type=multiinfo" -s)
			set -l names (echo $tmp | jshon -e results -a -e Name -u)
			set -l descs (echo $tmp | jshon -e results -a -e Description -u)
			set -l urls (echo $tmp | jshon -e results -a -e URL -u)
			set -l versions (echo $tmp | jshon -e results -a -e Version -u)
			set -l deps (echo $tmp | jshon -e results -a -e Depends -a -u -Q | sort -u)
			set -l makedeps (echo $tmp | jshon -e results -a -e MakeDepends -a -u -Q | sort -u)
			# set -l aurdeps (pacman -Si (string replace -ar '[>=<].*$' '' -- $deps $makedeps) >/dev/null ^| cut -d"'" -f2)
			# This should handle versioned deps and providers
			set -l aurdeps (pacman -Spq -- $deps $makedeps >/dev/null ^| string replace -r '^.*: ' '')
			set -l cloneurls "https://aur.archlinux.org/"(echo $tmp | jshon -e results -a -e PackageBase -u)".git"
			set -l clonenum 1
			echo "Aurdeps: $aurdeps"
			for pkg in $aurdeps
				echo "Now cloning: $pkg"
				aur clone (string replace -ar '[>=<].*$' '' -- $pkg)
			end
			git clone $cloneurls[$clonenum] $aurqueue/$names
		end
		return 0
	else if [ $mode = "rm" ]
		rm -rf $aurqueue/$argv
	else if [ $mode = "promote" ]
		mv $aurqueue/$argv $aurpkgs
		for pkg in $argv
			git -C $aurpkgs submodule add ./$pkg
		end
	else if [ $mode = "demote" ]
		echo "Demote mode is not implemented yet"
	# TODO:
	# else if [ $mode = "build" ]
	# 	# First clone, then recursively build AUR deps, then build
	else if [ $mode = "update" ]
		git -C $aurpkgs submodule foreach git pull origin master
		makepkgs $aurpkgs
	end
end
