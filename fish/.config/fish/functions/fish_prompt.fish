function fish_prompt
    set -l last_status $status

    set -l normal (set_color normal)
    set -l usercolor (set_color $fish_color_user)

    set -l delim \U25BA
    # If we don't have unicode use a simpler delimiter
    string match -qi "*.utf-8" -- $LANG; or set delim ">"

    # CWD colored red if we are root.
    set -l cwd (set_color $fish_color_cwd)
    test $USER = root; and set -q fish_color_cwd_root
    and set cwd (set_color $fish_color_cwd_root)

    # Prompt status only if it's not 0
    set -l prompt_status
    test $last_status -ne 0; and set prompt_status (set_color $fish_color_error)"[$last_status]$normal" # 😉

    # Only show host if in SSH or container
    # Store this in a global variable because it's slow and unchanging
    if not set -q prompt_host
        set -g prompt_host ""
        set -g prompt_host_nocolor ""
        if set -q SSH_TTY
            or begin command -sq systemd-detect-virt
                and systemd-detect-virt -q
            end
            set -l host (hostname)
            set prompt_host_nocolor $USER@$host
            set prompt_host $usercolor$USER$normal@(set_color $fish_color_host)$host$normal":"
        end
    end

    # Shorten pwd if prompt is too long
    set -l pwd (prompt_pwd)
    # This is quite cheesy - we simply try all prompt_pwds until it fits.
    # Since prompt_pwd is builtins-only, this is usually quite fast.
    # 0 means unshortened
    set -l seq $fish_prompt_pwd_dir_length 0 10 9 8 7 6 5 4 3 2 1
    for i in $seq
        set pwd (prompt_pwd $i)
        set -l len (string length -- $prompt_host_nocolor$pwd$last_status$delim' ')
        if test $len -lt $COLUMNS
            break
        end
    end
    echo -n -s $prompt_host $cwd $pwd $normal $prompt_status $delim ' '
end
