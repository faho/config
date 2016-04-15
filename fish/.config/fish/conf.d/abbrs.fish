if not set -q fish_initialized
	abbr -a alsamixer alsamixer -c0
	abbr -a e emacs -nw
	abbr -a \$EDITOR $EDITOR
	abbr -a \$PAGER less
	abbr -a mu4e emacs --eval "\(mu4e\)"
	abbr -a pm pulsemixer
	abbr -a rm rm -I
	abbr -a sc systemctl
	abbr -a upo upower -i /org/freedesktop/UPower/devices/battery_BAT0
	abbr -a usc systemctl --user
	# Double-escaping needed
	abbr -a d2 env WINEPREFIX=/home/alfa/.wine32/ wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Diablo\\ II/Diablo\\ II.exe
	abbr -a c curl -LO -C -
	set -U  fish_user_paths ~alfa/.local/bin $GOPATH/bin
	set -U fish_initialized
end
