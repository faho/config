function fish_prompt
	set -l last_status $status

    set -l normal (set_color normal)
	set -l usercolor (set_color $fish_color_user)

	set -l delim \U25BA
	# If we don't have unicode use a simpler delimiter
	string match -qi "*.utf-8" -- $LANG; or set delim ">"

	set -l cwd (set_color $fish_color_cwd)
	test $USER = root; and set -q fish_color_cwd_root
    and set cwd (set_color $fish_color_cwd_root)

    test $last_status -ne 0; and set -l prompt_status (set_color $fish_color_status)"[$last_status]$normal"

	# Only show host if in SSH or container
	# Store this in a global variable because it's slow and unchanging
	set -q prompt_host; or begin set -g prompt_host ""
		if set -q SSH_TTY
			or begin type -q systemd-detect-virt
				and systemd-detect-virt -q
			end
			set prompt_host $usercolor$USER$normal@(set_color $fish_color_host)(hostname)$normal":"
		end
	end

	# Shorten pwd if prompt is too long
	# TODO: This will break if prompt_host is actually used because of the control sequences
	set -l pwd (prompt_pwd)
	set -l len (string length -- $prompt_host$pwd$last_status$delim' ')
	if test $len -gt $COLUMNS
		set pwd (string replace -ar '([^/])[^/]*' '$1' -- $pwd)
	end
    echo -n -s $prompt_host $cwd $pwd $normal $prompt_status $delim ' '
end
