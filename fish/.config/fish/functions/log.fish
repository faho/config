function log
	# Only messages from this boot,
	# priority 0 (emerg) to 6 (info) (i.e. excluding 7 - debug because that's too wordy)
	# And follow after showing the last 1000 lines
	journalctl -b -f -n 1000 --priority 0..6
end
