function faho_getaurdeps
	# Read dependencies in .SRCINFO format
	set -l deps
	while read -l -d '=' key value
		string match -q -- "*depends" $key; and set deps $deps (string replace -r ':.*' '' -- $value)
	end
	# This should handle versioned deps and providers
	test -n "$deps"; and if set -l aurdeps (pacman -Spq $deps >/dev/null 2>| string replace -r '.*: ' '')
		printf "%s\n" $aurdeps
		return 0
	end
	return 1
end
