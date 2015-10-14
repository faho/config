function fish_right_prompt
	type -q __fish_vcs_prompt; and __fish_vcs_prompt
	set -l bat (battery)
	set -l plug
	set -q BATTERY_IS_PLUGGED; and set plug "⚡"
	set -l d (date "+%R")
	printf "%s%s %s" $plug $bat $d
end
