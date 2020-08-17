function fzf_complete
    set -l cmdline (commandline)
    # HACK: Color descriptions manually.
    complete -C | string replace -r \t'(.*)$' \t(set_color $fish_pager_color_description)'$1'(set_color normal) \
        | sort -u | fzf -d \t -1 -0 --ansi --header="$cmdline" --height="80%" --tabstop=4 \
        | read -l token
    # Remove description
    set token (string replace -r \t'.*' '' -- $token)
    commandline -rt "$token"
end
