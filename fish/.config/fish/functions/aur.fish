function aur --description 'Quite possibly the stupidest aur helper ever invented'
	# TODO: This does not correctly handle searches with multiple arguments
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
			set -l resultcount (echo $tmp | jshon -e resultcount)
			if [ $resultcount -eq "0" ]
				echo $red"No results found" >&2
				return 1
			end
			set -l names (echo $tmp | jshon -e results -a -e Name -u)
			set -l descs (echo $tmp | jshon -e results -a -e Description -u)
			set -l urls (echo $tmp | jshon -e results -a -e URL -u)
			set -l versions (echo $tmp | jshon -e results -a -e Version -u)
			for i in (seq $resultcount)
				printf "%s %s\n\t%s (%s)\n" $bold$names[$i]$normal $green$versions[$i]$normal $descs[$i] $yellow$urls[$i]$normal
			end
			return 0
		case clone download
			for pkg in $argv
				set -l target
				if string match -q "*/*" -- $pkg
					set target (string split "/" -- $pkg)[1]
					if not contains -- $target pkgs queue
						echo $red"Invalid target: $target from argument $pkg" >&2
						return 1
					end
					set pkg (string split "/" -- $pkg)[2]
				end
				set -l tmp (curl -G --data-urlencode "arg=$pkg" "$aurl&type=info" -s)
				if [ (echo $tmp | jshon -e resultcount) -eq "0" ]
					echo $red"No results found" >&2
					return 1
				end
				set -l names (echo $tmp | jshon -e results -a -e Name -u)
				set -l descs (echo $tmp | jshon -e results -a -e Description -u)
				set -l urls (echo $tmp | jshon -e results -a -e URL -u)
				set -l versions (echo $tmp | jshon -e results -a -e Version -u)
				set -l deps (echo $tmp | jshon -e results -a -e Depends -a -u -Q | sort -u)
				set -l makedeps (echo $tmp | jshon -e results -a -e MakeDepends -a -u -Q | sort -u)
				test -n "$deps$makedeps"
				and set -l aurdeps (printf '%s\n' $deps $makedeps | faho_getaurdeps)
				set -l cloneurls "https://aur.archlinux.org/"(echo $tmp | jshon -e results -a -e PackageBase -u)".git"
				set -l clonenum 1
				echo "Aurdeps: $aurdeps"
				# TODO: This could be a recursive mode
				# for pkg in $aurdeps
				# 	echo "Now cloning: $pkg"
				# 	aur clone (string replace -ar '[>=<].*$' '' -- $pkg)
				# end
				[ "$target" != pkgs ]; and set dir $aurqueue/$names
				or set dir $aurpkgs/$names
				[ ! -e "$dir" ]; and git clone $cloneurls[$clonenum] $dir
			end
			return 0
		case info
			for pkg in $argv
				set -l tmp (curl -G --data-urlencode "arg=$pkg" "$aurl&type=info" -s)
				set -l resultcount (echo $tmp | jshon -e resultcount)
				if [ $resultcount -eq "0" ]
					echo $red"No results found" >&2
					return 1
				end
				set -l names (echo $tmp | jshon -e results -a -e Name -u)
				set -l descs (echo $tmp | jshon -e results -a -e Description -u)
				set -l urls (echo $tmp | jshon -e results -a -e URL -u)
				set -l versions (echo $tmp | jshon -e results -a -e Version -u)
				set -l deps (echo $tmp | jshon -e results -a -e Depends -a -u -Q | sort -u)
				set -l makedeps (echo $tmp | jshon -e results -a -e MakeDepends -a -u -Q | sort -u)
				test -n "$deps$makedeps"
				and set -l aurdeps (printf '%s\n' $deps $makedeps | faho_getaurdeps)
				for d in $aurdeps
					if set -l i (contains -i -- $d $deps)
						set -e deps[$i]
					end
					if set -l i (contains -i -- $d $makedeps)
						set -e makedeps[$i]
					end
				end

				for i in (seq $resultcount)
					printf "$bold%-32s:$normal %s\n" "Name" $names[$i] "Description" $descs[$i] "Version" $versions[$i] "Url: " $urls[$i] \
					"Dependencies" "$deps" "Makedependencies" "$makedeps" "(Make)-Dependencies from AUR" "$aurdeps"
				end
			end
			return 0
		case rm
			[ -n "$argv" ]
			for pkg in $argv
				set -l dir (aur_findpkg $pkg)
				if test -z "$dir"
					echo $red"No such package: "$normal"$pkg" >&2
					return 4
				end
				rm -rf $dir
			end
		case promote
			mv $aurqueue/$argv $aurpkgs
			for pkg in $argv
				git -C $aurpkgs submodule add ./$pkg
			end
		case demote
			for pkg in $argv
				[ -d "$aurpkgs/$pkg" ]
				and mv $aurpkgs/$pkg $aurqueue/$pkg
				# We've moved it away so we can --force to remove the tree
				and git -C $aurpkgs submodule deinit --force $pkg
			end
			return 0
		case build install
			for pkg in $argv
				set -l dir (aur_findpkg $pkg)
				# If necessary, clone it
				if [ -z "$dir" ]
					aur clone $pkg; or return 1
					string match -q 'pkgs/*' -- $pkg; and set dir $aurpkgs/$pkg
					or set dir $aurqueue/$pkg
				end
				if not test -e $dir/.SRCINFO
					echo $red"SRCINFO does not exist" >&2
					echo "Please ensure $dir is non-existent or a valid package clone"$normal >&2
					return 3
				end
				# Parse SRCINFO for deps
				set -l aurdeps (test -r .SRCINFO; or makepkg --printsrcinfo > .SRCINFO; faho_getaurdeps < .SRCINFO)
				[ -n "$aurdeps" ]; and for dep in (string replace -ar '[>=<].*$' '' -- $aurdeps)
					echo "Building $dep"
					aur build $dep
				end
				makepkgs $dir
			end
			return 0
		case update
			if set -q argv[1]
				set -l packages
				for pkg in $argv
					set -l dir (aur_findpkg $pkg)
					if test -z "$dir"
						echo "No such package $pkg in $target"
						return 5
					end
					echo ADDING PACKAGE $dir
					set packages $packages $dir
				end
				for pkg in $packages
					git -C $pkg pull origin master
				end
				makepkgs $packages
			else
				git -C $aurpkgs submodule foreach git pull origin master
				makepkgs $aurpkgs/*
			end
		case list ls
			set -l printqueue
			set -l printpkgs
			set -q argv[1]; and switch $argv[1]
				case "--queueonly"
					set -e printpkgs
				case "--pkgsonly"
					set -e printqueue
			end
			if set -q printqueue printpkgs
				for i in (string replace -- "$aurqueue/" "" $aurqueue/*)
					echo queue/$i
				end
				for i in (string replace -- "$aurpkgs/" "" $aurpkgs/*)
					echo pkgs/$i
				end
			else
				set -q printqueue; and ls $aurqueue
				set -q printpkgs; and ls $aurpkgs
			end
		case log
			for pkg in $argv
				set -l dir (aur_findpkg $pkg)
				# If necessary, clone it
				if [ -z "$dir" ]
					aur clone $pkg
					set dir $aurqueue/$pkg
				end
				test -n "$dir"; and git -C $dir log
			end
		case show
			for pkg in $argv
				set -l dir (aur_findpkg $pkg)
				# If necessary, clone it
				if [ -z "$dir" ]
					aur clone $pkg
					and set dir $aurqueue/$pkg
				end
				test -n "$dir"; and eval $EDITOR (string escape -- $dir)
			end
		case "help" "*" ""
			echo "Usage: aur <Operation> [...]"
			echo "Operations:"
			echo "search <keywords>"
			echo "clone <Packages>"
			echo "promote <Packages>"
			echo "demote <Packages>"
			echo "rm <Packages>"
			echo "build <Packages>"
			echo "log <Packages>"
			echo "update"
			echo "show <Packages>"
			return 0
	end
end
