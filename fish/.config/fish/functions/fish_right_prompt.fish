function fish_right_prompt
	# set -l battery_percentage (upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage | cut -d":" -f2 | cut -d"%" -f1)
	# set -l battery (bar $battery_percentage)
	set -l bat (battery)
	set -l plug
	set -q BATTERY_IS_PLUGGED; and set plug "⚡"
	set -l d (date "+%R %A %F")
	printf "%s%s %s" $plug $bat $d
end
