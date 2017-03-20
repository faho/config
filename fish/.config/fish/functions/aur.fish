function aur --description 'Quite possibly the stupidest aur helper ever invented'
    # TODO: This does not correctly handle searches with multiple arguments
    # TODO: These should be real configuration
    set -q aurqueue
    or set -l aurqueue ~/dev/build/new
    set -q aurpkgs
    or set -l aurpkgs ~/dev/build/current
    # For some reason only explicitly encoding the version gets us deps
    set -q aurl
    or set -l aurl "https://aur.archlinux.org/rpc.php?v=5"

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
            if not set -q argv[1]
                echo $red"Please supply at least one keyword" >&2
                return 1
            end
            # A "--name-only" option to sort only by name
            set -l nameonly
            if set -l ind (contains -i -- --name-only $argv)
                set -e argv[$ind]
                set nameonly "&by=name"
            end
            set -l arg --data-urlencode "arg=$argv[1]"
            set -e argv[1]
            # This line dominates the profile, the jshon calls don't matter
            set -l tmp (curl -G $arg "$aurl&type=search$nameonly" -s)
            if test $status -gt 0
                echo $red"Could not contact AUR" >&2
                return 1
            end
            set -l resultcount (echo $tmp | jshon -e resultcount)
            if test $resultcount -eq "0"
                echo $red"No results found" >&2
                return 1
            end
            set -l names (echo $tmp | jshon -e results -a -e Name -u)
            set -l descs (echo $tmp | jshon -e results -a -e Description -u)
            set -l urls (echo $tmp | jshon -e results -a -e URL -u)
            set -l versions (echo $tmp | jshon -e results -a -e Version -u)
            for i in (seq $resultcount)
                # Unfortunately the AUR rpc does not support multiple search arguments
                # So we have to do searches for everything but the first ourselves
                set -l printp
                # This will only be called with search terms after the first (since we deleted that)
                for a in $argv
                    if not string match -q "*$a*" -- $names[$i] (set -q nameonly[1]; or $descs[$i])
                        set -e printp
                        break
                    end
                end

                if set -q printp
                    printf "%s %s\n  %s (%s)\n" $bold$names[$i]$normal $green$versions[$i]$normal $descs[$i] $yellow$urls[$i]$normal
                end
            end
            echo "Found $bold$resultcount$normal results" >&2
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
                if test $status -gt 0
                    echo $red"Could not contact AUR" >&2
                    return 1
                end
                if test (echo $tmp | jshon -e resultcount) -eq "0"
                    echo $red"No results found" >&2
                    return 1
                end
                set -l names (echo $tmp | jshon -e results -a -e Name -u)
                set -l descs (echo $tmp | jshon -e results -a -e Description -u)
                set -l urls (echo $tmp | jshon -e results -a -e URL -u)
                set -l versions (echo $tmp | jshon -e results -a -e Version -u)
                set -l deps (echo $tmp | jshon -e results -a -e Depends -a -u -Q | sort -u)
                set -l makedeps (echo $tmp | jshon -e results -a -e MakeDepends -a -u -Q | sort -u)
                set -l aurdeps (printf '%s\n' $deps $makedeps | faho_getaurdeps)
                set -l cloneurls "https://aur.archlinux.org/"(echo $tmp | jshon -e results -a -e PackageBase -u)".git"
                set -l clonenum 1
                echo "Aurdeps: $aurdeps"
                # TODO: This could be a recursive mode
                # for pkg in $aurdeps
                # 	echo "Now cloning: $pkg"
                # 	aur clone (string replace -ar '[>=<].*$' '' -- $pkg)
                # end
                test "$target" != pkgs
                and set dir $aurqueue/$names
                or set dir $aurpkgs/$names
                test -e "$dir"
                or git clone $cloneurls[$clonenum] $dir
            end
            return 0
        case info
            for pkg in $argv
                set -l tmp (curl -G --data-urlencode "arg=$pkg" "$aurl&type=info" -s)
                if test $status -gt 0
                    echo "Could not contact AUR" >&2
                    return 1
                end
                set -l resultcount (echo $tmp | jshon -e resultcount)
                if test $resultcount -eq "0"
                    echo $red"No results found" >&2
                    return 1
                end
                set -l names (echo $tmp | jshon -e results -a -e Name -u)
                set -l descs (echo $tmp | jshon -e results -a -e Description -u)
                set -l urls (echo $tmp | jshon -e results -a -e URL -u)
                set -l versions (echo $tmp | jshon -e results -a -e Version -u)
                set -l deps (echo $tmp | jshon -e results -a -e Depends -a -u -Q | sort -u)
                set -l makedeps (echo $tmp | jshon -e results -a -e MakeDepends -a -u -Q | sort -u)
                set -l aurdeps (printf '%s\n' $deps $makedeps | faho_getaurdeps)
                for d in $aurdeps
                    if set -l i (contains -i -- $d $deps)
                        set -e deps[$i]
                    end
                    if set -l i (contains -i -- $d $makedeps)
                        set -e makedeps[$i]
                    end
                end

                for i in (seq $resultcount)
                    printf "$bold%-32s:$normal %s\n" "Name" $names[$i] "Description" $descs[$i] "Version" $versions[$i] "Url: " $urls[$i] "Dependencies" "$deps" "Makedependencies" "$makedeps" "(Make)-Dependencies from AUR" "$aurdeps"
                end
            end
            return 0
        case rm
            for pkg in $argv
                set -l dir (aur_findpkg $pkg)
                if not set -q dir[1]
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
                and git -C $aurpkgs rm -f $pkg
            end
            return 0
        case build install
            set -l opts
            set -l pkgs
            # Pass all options along to makepkg
            for arg in $argv
                if string match -qr -- '^-.*' $arg
                    set opts $opts $arg
                else
                    set pkgs $pkgs $arg
                end
            end
            for pkg in $pkgs
                set -l dir (aur_findpkg $pkg)
                # If necessary, clone it
                if not set -q dir[1]
                    aur clone $pkg $opts
                    or return 1
                    string match -q 'pkgs/*' -- $pkg
                    and set dir $aurpkgs/$pkg
                    or set dir $aurqueue/$pkg
                end
                # Parse SRCINFO for deps
                set -l aurdeps (test -r $dir/.SRCINFO; or makepkg --printsrcinfo > $dir/.SRCINFO; faho_getaurdeps < $dir/.SRCINFO)
                set -q aurdeps[1]
                and for dep in (string replace -ar '[>=<].*$' '' -- $aurdeps)
                    echo "Building $dep"
                    aur build $dep $opts
                end
                makepkgs $dir $opts
            end
            return 0
        case update
            set -l packages
            if set -q argv[1]
                for pkg in $argv
                    set -l dir (aur_findpkg $pkg)
                    if not set -q dir[1]
                        echo "No such package $pkg in $target"
                        return 5
                    end
                    set packages $packages $dir
                end
            else
                set packages $aurpkgs/*
            end
            set -l failedpulls
            for pkg in $packages
                # If PKGBUILD only differs in the pkgver= lines, check it out.
                # This is because makepkg updates that line in-file for VCS packages, resulting in failed merges.
                if git -C $pkg diff -- PKGBUILD | string match -r '^[+-][^+-].*' | string match -rv '^[+-]pkgver=' | not string length -q
                    git -C $pkg checkout -- PKGBUILD
                end
                git -C $pkg pull origin master
                or set failedpulls $failedpulls $pkg
            end
            if not set -q failedpulls[1]
                makepkgs $packages
            else
                echo "The following packages failed pulling:" >&2
                string replace -- "$aurpkgs/" "pkgs/" $failedpulls >&2
                echo "Fix the errors and rerun" >&2
                return 6
            end
        case list
            set -l printqueue
            set -l printpkgs
            set -q argv[1]
            and switch $argv[1]
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
                set -q printqueue
                and ls $aurqueue
                set -q printpkgs
                and ls $aurpkgs
            end
        case log
            for pkg in $argv
                set -l dir (aur_findpkg $pkg)
                # If necessary, clone it
                if not set -q dir[1]
                    aur clone $pkg
                    and set dir $aurqueue/$pkg
                end
                set -q dir[1]
                and git -C $dir log
            end
        case show
            for pkg in $argv
                set -l dir (aur_findpkg $pkg)
                # If necessary, clone it
                if not set -q dir[1]
                    aur clone $pkg
                    and set dir $aurqueue/$pkg
                end
                set -q dir[1]
                and eval $EDITOR (string escape -- $dir)
            end
        case "cd"
            set -l dir (aur_findpkg $argv[1])
            if set -q dir[1]
                cd $dir
            end
        case "help" "*"
            echo $bold"aur.fish $normal -- The stupidest aur helper"
            echo
            echo $bold"Usage:$normal aur <Operation> [...]"
            echo
            echo $bold"Operations:$normal"
            echo $bold"    search$normal [--name-only] <Keywords>"
            echo "         Search the AUR for packages matching all given keywords in name or description"
            echo $bold"    clone$normal <Packages>"
            echo "         Clone packages to the queue directory"
            echo $bold"    promote$normal <Packages>"
            echo "         Move a package from the queue to the pkg directory"
            echo $bold"    demote$normal <Packages>"
            echo "         Move a package from the pkg to the queue directory"
            echo $bold"    rm$normal <Packages>"
            echo "         Remove a package from the queue directory"
            echo $bold"    build$normal <Packages>"
            echo "         Build and install packages"
            echo $bold"    log$normal <Packages>"
            echo "         Show the git log for packages"
            echo $bold"    update$normal <Packages>"
            echo "         Update and install all given packages"
            echo $bold"    show$normal <Packages>"
            echo "         Open the package's directory in \$EDITOR"
            echo $bold"    list$normal"
            echo "         List packages"
            return 0
    end
end

function aur_findpkg
	if set -q argv[2]
		echo "Please only pass one argument" >&2
		return 2
	end
	set -q aurqueue; or set -l aurqueue ~/dev/build/new
	set -q aurpkgs; or set -l aurpkgs ~/dev/build/current

	set -l dir
	if not string match -q "pkgs/*" -- $argv
		set -l d "$aurqueue/"(string replace -r "^queue/" "" -- $argv)
		test -d "$d"; and set dir "$d"
	end
	if not string match -q "queue/*" -- $argv
		set -l d "$aurpkgs/"(string replace -r "^pkgs/" "" -- $argv)
		test -d "$d"; and set dir "$d"
	end
	if test -n "$dir"
		echo $dir
		return 0
	else
		return 1
	end
end
