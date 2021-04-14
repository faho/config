function fish_right_prompt
    set -l vcs (fish_vcs_prompt 2>/dev/null)

    type -q battery
    and set -l bat (battery)
    set -l plug ""
    set -q BATTERY_IS_PLUGGED
    and set plug "⚡"

    set -l d (set_color brgrey)(date "+%R")(set_color normal)

    set -l duration "$cmd_duration$CMD_DURATION"
    if test $duration -gt 100
        set duration (math $duration / 1000)s
    else
        set duration ""
    end

    # A simpler version for stupid locales
    if not string match -qir '.*\.utf-?8' -- $LANG
        set -q BATTERY_IS_PLUGGED
        and set plug p
        set bat (printf '%03d%%\n' $BATTERY_PCT)
    end

    if test "$SHLVL" -ge 2
        printf "SUBSHELL $SHLVL "
    end

    set -q VIRTUAL_ENV_DISABLE_PROMPT
    or set -g VIRTUAL_ENV_DISABLE_PROMPT true

    set -q VIRTUAL_ENV
    and set -l venv (string replace -r '.*/' '' -- "$VIRTUAL_ENV")

    set_color reset
    printf "%s %s %s %s%s %s" $venv $duration "$vcs" "$plug" "$bat" "$d"
end
