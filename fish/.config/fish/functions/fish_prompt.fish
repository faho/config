function fish_prompt --description 'Prompt anzeigen'
	set -l last_status $status

    set -g __fish_prompt_hostname (hostname)
    set -g __fish_prompt_normal (set_color normal)

    set -l delim '➤'

    switch $USER
        case root
            if set -q fish_color_cwd_root
                set -g __fish_prompt_cwd (set_color $fish_color_cwd_root)
            else
                set -g __fish_prompt_cwd (set_color $fish_color_cwd)
            end
        case '*'
            set -g __fish_prompt_cwd (set_color $fish_color_cwd)
    end

    set -l prompt_status
    if test $last_status -ne 0
        set -g __fish_prompt_status (set_color $fish_color_status)
        set prompt_status "$__fish_prompt_status [$last_status]$__fish_prompt_normal"
    end

    set -l host
    if begin set -q SSH_TTY
            or begin type -q systemd-detect-virt
                and systemd-detect-virt -q
            end
        end
        set -g __fish_prompt_user (set_color $fish_color_user)
        set -g __fish_prompt_host (set_color $fish_color_host)
        set host "$__fish_prompt_user$USER$__fish_prompt_normal@$__fish_prompt_host$__fish_prompt_hostname$__fish_prompt_normal "
    end

    echo -n -s $host "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" "$prompt_status" "$delim" ' '
end
