function fish_right_prompt
    set -l note "♪"
    type -q fish_vcs_prompt
    and set -l vcs (fish_vcs_prompt 2>/dev/null)
    set -l bat (battery)
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
        set note ""
        set -q BATTERY_IS_PLUGGED
        and set plug "p"
        set bat (printf '%03d%%\n' $BATTERY_PCT)
    end
    set -l mpc
    # if type -q mpc; and systemctl --user --quiet is-active mpd >/dev/null 2>/dev/null
    # 	if set mpc (mpc status 2>/dev/null)
    # 		if set -q mpc[2]; and string match -q "[playing]*" -- $mpc[2]
    # 			set mpc (set_color brcyan)"$note$mpc[1]$note"(set_color normal)
    # 		else
    # 			set mpc ""
    # 		end
    # 	end
    # end
    if test "$SHLVL" -ge 2
        printf "SUBSHELL $SHLVL "
    end

    set -q VIRTUAL_ENV_DISABLE_PROMPT
    or set -g VIRTUAL_ENV_DISABLE_PROMPT true
    set -q VIRTUAL_ENV
    and set -l venv (string replace -r '.*/' '' -- "$VIRTUAL_ENV")

    printf "%s %s %s %s%s %s" $venv $duration "$vcs" $plug $bat $d
end
