function fish_right_prompt
    set -l vcs (fish_vcs_prompt 2>/dev/null)

    set -l d (set_color brgrey)(date "+%R")(set_color normal)

    set -l duration "$cmd_duration$CMD_DURATION"
    if test $duration -gt 100
        set duration (math $duration / 1000)s
    else
        set duration
    end

    set -l shlvl ""
    if test "$SHLVL" -ge 2
        set shlvl "SUBSHELL $SHLVL"
    end

    set -q VIRTUAL_ENV_DISABLE_PROMPT
    or set -g VIRTUAL_ENV_DISABLE_PROMPT true
    set -q VIRTUAL_ENV
    and set -l venv (string replace -r '.*/' '' -- "$VIRTUAL_ENV")

    set_color reset
    string join " " -- $shlvl $venv $duration $vcs $d
end
