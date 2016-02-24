function fish_title
	test $USER = "root"; and set -l user root
	echo -n $user $PWD: $argv[1]
end
