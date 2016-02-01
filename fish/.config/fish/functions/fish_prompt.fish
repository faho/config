function fish_prompt --description 'Prompt anzeigen'
	set -l last_status $status

    set -l normal (set_color normal)
	set -l usercolor (set_color $fish_color_user)

    # set -l delim '➤'
	set -l delim "❯"

	set -l cwd (set_color $fish_color_cwd)
	test $USER = root; and set -q fish_color_cwd_root
    and set cwd (set_color $fish_color_cwd_root)

    test $last_status -ne 0; and set -l prompt_status (set_color $fish_color_status)"[$last_status]$normal"

	# Only show host if in SSH or container
	# Cache this in a global variable
	set -q prompt_host; or begin set -g prompt_host
		if set -q SSH_TTY
			or begin type -q systemd-detect-virt
				and systemd-detect-virt -q
			end
			set prompt_host $usercolor$USER$normal@(set_color $fish_color_host)(hostname)(set_color normal)":"
		end
	end

    echo -n -s $prompt_host $cwd (prompt_pwd) $normal $prompt_status $delim ' '
end
