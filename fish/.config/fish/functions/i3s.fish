function i3s
	battery > /dev/null
	while true
		battery_update_info
		set -l bat (string trim -- $BATTERY_PCT%)
		set -l date (date "+%A %H:%M %d-%m-%y")
		printf ' %s' $bat $date
		sleep 5s
	end
end
