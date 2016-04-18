function faho_getaurdeps
	# Read dependencies in .SRCINFO format
	set -l deps
	# Yes, the "eq" will be set to "="
	while read -l key eq value
		string match -q -- "*depends" $key; and set deps $deps (string replace -r ':.*' '' -- $value)
	end
	# This should handle versioned deps and providers
	test -n "$deps"; and if set -l aurdeps (pacman -Spq $deps >/dev/null ^| string replace -r '.*: ' '')
		printf "%s\n" $aurdeps
		return 0
	end
	return 1
end
