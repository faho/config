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
