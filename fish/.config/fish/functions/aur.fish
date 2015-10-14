function aur --description 'Quite possibly the stupidest aur helper ever invented'
	# TODO: This does not correctly handle searches with multiple arguments
	# TODO: Implement "demote" (move from current to new)
	# TODO: These should be real configuration
	set -q aurqueue; or set -l aurqueue ~/dev/build/new
	set -q aurpkgs; or set -l aurpkgs ~/dev/build/current

	# Enable color when in terminal
	set -l yellow ""
	set -l blue ""
	set -l red ""
	set -l normal ""
	set -l bold ""
	set -l green ""
	if isatty stdout
		set yellow (set_color yellow)
		set normal (set_color normal)
		set blue (set_color blue)
		set red (set_color red)
		set bold (set_color --bold)
		set green (set_color green)
	end

	# Parse arguments
	set -l mode
	switch "$argv[1]"
		case "search" "clone" "promote" "demote" "rm" "build" "info"
			set mode $argv[1]
			set -e argv[1]
		case "update"
			set mode update
		case "help" "*" ""
			echo "Usage: aur <Operation> [...]"
			echo "Operations:"
			echo "search <keywords>"
			echo "clone <Packages>"
			echo "promote <Packages>"
			echo "demote <Packages>"
			echo "rm <Packages>"
			echo "build <Packages>"
			echo "update"
			return 0
	end
			
	if not set -q argv[1]
		echo "More arguments needed, dumbass"
		return 1
	end

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
			printf "%s %s\n\t%s (%s)\n" $bold$names[$i]$normal $green$versions[$i]$normal $descs[$i] $yellow$urls[$i]$normal
		end
		return 0
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
		# TODO: This could be a recursive mode
		# for pkg in $aurdeps
		# 	echo "Now cloning: $pkg"
		# 	aur clone (string replace -ar '[>=<].*$' '' -- $pkg)
		# end
		[ ! -e $aurqueue/$names ]; and git clone $cloneurls[$clonenum] $aurqueue/$names
		return 0
	else if [ $mode = info ]
		if set -q argv[2]
			echo "Too many arguments - info takes only one"
			return 1
		end
		# For some reason only explicitly encoding the version gets us deps
		set -l tmp (curl -G --data-urlencode "arg=$argv" "https://aur.archlinux.org/rpc.php?v=4&type=info" -s)
		set -l names (echo $tmp | jshon -e results -a -e Name -u)
		set -l descs (echo $tmp | jshon -e results -a -e Description -u)
		set -l urls (echo $tmp | jshon -e results -a -e URL -u)
		set -l versions (echo $tmp | jshon -e results -a -e Version -u)
		set -l deps (echo $tmp | jshon -e results -a -e Depends -a -u -Q | sort -u)
		set -l makedeps (echo $tmp | jshon -e results -a -e MakeDepends -a -u -Q | sort -u)
		set -l aurdeps (pacman -Spq -- $deps $makedeps >/dev/null ^| string replace -r '^.*: ' '')
		for d in $aurdeps
			if set -l i (contains -i -- $d $deps)
				set -e deps[$i]
			end
			if set -l i (contains -i -- $d $makedeps)
				set -e makedeps[$i]
			end
		end

		for i in (seq (count $names))
			printf "$bold%-32s:$normal %s\n" "Name" $names[$i] "Description" $descs[$i] "Version" $versions[$i] "Url: " $urls[$i] \
			"Dependencies" "$deps" "Makedependencies" "$makedeps" "(Make)-Dependencies from AUR" "$aurdeps"
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
		return 1
	else if [ $mode = "build" ]
		for pkg in $argv
			# HACK: This is _really_ ugly
			# It should parse .SRCINFO instead
			set -l aurdeps (aur clone $argv | string match "Aurdeps:*" | string replace "Aurdeps: " "" | string trim | string split " ")
			[ -n "$aurdeps" ]; and for dep in (string replace -ar '[>=<].*$' '' -- $aurdeps)
				echo "Building $dep"
				aur build $dep
			end
			[ -d $aurpkgs/$pkg ]; and set -l dir $aurpkgs/$pkg
			[ -d $aurqueue/$pkg ]; and set -l dir $aurqueue/$pkg
			echo "Making $dir"
			makepkgs $dir
		end
		return 0
	else if [ $mode = "update" ]
		git -C $aurpkgs submodule foreach git pull origin master
		makepkgs $aurpkgs/*
	end
end
