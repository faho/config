function aur --description 'Quite possibly the stupidest aur helper ever invented'
	# TODO: This does not correctly handle searches with multiple arguments
	# TODO: Implement "demote" (move from current to new)
	# TODO: These should be real configuration
	set -q aurqueue; or set -l aurqueue ~/dev/build/new
	set -q aurpkgs; or set -l aurpkgs ~/dev/build/current
	# For some reason only explicitly encoding the version gets us deps
	set -q aurl; or set -l aurl "https://aur.archlinux.org/rpc.php?v=4"

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

	set -l mode $argv[1]
	set -e argv[1]
	switch "$mode"
		case search
			set -l arg
			for a in $argv
				set arg --data-urlencode "arg=$a" $arg
			end
			# This line dominates the profile, the jshon calls don't matter
			set -l tmp (curl -G $arg "$aurl&type=search" -s)
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
		case clone
			for pkg in $argv
				set -l tmp (curl -G --data-urlencode "arg=$pkg" "$aurl&type=info" -s)
				if [ (echo $tmp | jshon -e resultcount) -eq "0" ]
					echo "No results found"
					return 1
				end
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
			end
			return 0
		case info
			for pkg in $argv
				set -l tmp (curl -G --data-urlencode "arg=$pkg" "$aurl&type=info" -s)
				if [ (echo $tmp | jshon -e resultcount) -eq "0" ]
					echo "No results found"
					return 1
				end
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
			end
			return 0
		case rm
			[ -n "$argv" ]
			and rm -rf $aurqueue/$argv
		case promote
			mv $aurqueue/$argv $aurpkgs
			for pkg in $argv
				git -C $aurpkgs submodule add ./$pkg
			end
		case demote
			for pkg in $argv
				[ -d $aurpkgs/$pkg ]
				and mv $aurpkgs/$pkg $aurqueue/$pkg
				and git -C $aurpkgs submodule deinit $pkg
			end
			return 0
		case build
			for pkg in $argv
			# Find the package, if it's already cloned
			set -l dir
			[ -d $aurpkgs/$pkg ]; and set dir $aurpkgs/$pkg
			[ -d $aurqueue/$pkg ]; and set dir $aurqueue/$pkg
			# If necessary, clone it
			if [ -z "$dir" ]
				aur clone $pkg
				set dir $aurqueue/$pkg
			end
			# Parse SRCINFO for deps
			set -l aurdeps (string match \t"depends =*" < $dir/.SRCINFO | string replace -ar ".*= " "")
			set aurdeps $aurdeps (string match \t"makedepends =*" < $dir/.SRCINFO | string replace -ar ".*= " "")
			set aurdeps (pacman -Spq -- $aurdeps >/dev/null ^| string replace -r '^.*: ' '')
			[ -n "$aurdeps" ]; and for dep in (string replace -ar '[>=<].*$' '' -- $aurdeps)
				echo "Building $dep"
				aur build $dep
			end
			echo "Making $dir"
			makepkgs $dir
			end
			return 0
		case update
			git -C $aurpkgs submodule foreach git pull origin master
			makepkgs $aurpkgs/*
		case list ls
			set -l printqueue
			set -l printpkgs
			set -q argv[1]; and switch $argv[1]
				case "--queueonly"
					set -e printpkgs
				case "--pkgsonly"
					set -e printqueue
			end
			set -q printqueue; and ls $aurqueue
			set -q printpkgs; and ls $aurpkgs
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
end
