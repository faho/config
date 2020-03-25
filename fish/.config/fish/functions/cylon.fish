function cylon
	set -l eye (set_color red)--(set_color green)--(set_color red)--(set_color normal)
	set -l direction right
	set -l field "     e     "
	set -l mode terminal
    # TODO: When input is a pipe,
    # we should quit once the other side terminates.
    # `read` will fail when we never receive any input,
    # and it will cause us to run faster when input comes faster.
	isatty stdin; or set mode wait
	while begin test $mode = terminal; and sleep 0.1s; end
		or begin test $mode = wait; and read -zn 1; end
		printf "\r[%s]" (string replace -- "e" "$eye" $field)
		string match -qr -- 'e$' $field; and set direction left
		string match -qr -- '^e' $field; and set direction right
		test $direction = right; and set field (string replace 'e ' ' e' $field)
		test $direction = left; and set field (string replace ' e' 'e ' $field)
	end
	printf "\n"
end
