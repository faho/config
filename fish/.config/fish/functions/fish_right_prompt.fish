function fish_right_prompt
	set -l note "♪"
	type -q __fish_vcs_prompt; and set -l vcs (__fish_vcs_prompt)
	set -l bat (battery)
	set -q BATTERY_IS_PLUGGED; and set -l plug "⚡"
	set -l d (set_color brgrey)(date "+%R")(set_color normal)
	# A simpler version for stupid locales
	if not string match -qir '.*\.utf-?8' -- $LANG
		set note ""
		set plug "p"
		set bat (printf '%03d%%\n' $BATTERY_PCT)
	end
	set -l mpc
	if type -q mpc; and systemctl --user --quiet is-active mpd >/dev/null ^/dev/null
		if set mpc (mpc status ^/dev/null)
			if set -q mpc[2]; and string match -q "[playing]*" -- $mpc[2]
				set mpc (set_color brcyan)"$note$mpc[1]$note"(set_color normal)
			else
				set mpc ""
			end
		end
	end
	printf "%s %s %s%s %s" $mpc $vcs $plug $bat $d
end
