#
# Init file for fish
#

set -gx GPGKEY 36EBBDB3
set -gx GPG_TTY (tty)
set -gx GOPATH ~/dev/go
set -gx MAIL $HOME/.mail
set -gx MAILDIR $MAIL
set -U  fish_user_paths ~alfa/.local/bin $GOPATH/bin
set -gx CDPATH . $HOME $HOME/Videos
set -gx WINEDEBUG "-all"
set -gx NO_AT_BRIDGE 1
set -gx EDITOR "emacs -nw"
set -gx MPD_HOST "/run/user/1000/mpd.socket"
set -gx SWT_GTK3 0
set -gx PAGER "less"
set -gx SSH_ASKPASS /usr/bin/ksshaskpass

# SDL mapping for PS3 controller
set -x SDL_GAMECONTROLLERCONFIG "030000004c0500006802000011010000,PS3 Controller,a:b14,b:b13,back:b0,dpdown:b6,dpleft:b7,dpright:b5,dpup:b4,guide:b16,leftshoulder:b10,leftstick:b1,lefttrigger:b8,leftx:a0,lefty:a1,rightshoulder:b11,rightstick:b2,righttrigger:b9,rightx:a2,righty:a3,start:b3,x:b15,y:b12,platform:Linux,"

# XDG
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_DATA_HOME "$HOME/.local/share"
set -x XDG_CACHE_HOME "$HOME/.cache"

set -x SSH_AUTH_SOCK "$XDG_CONFIG_HOME/gnupg/S.gpg-agent.ssh"
set -x LESSHISTFILE "-"

set -x ASPROOT ~/packages/asp

set FISH_CLIPBOARD_CMD "cat"

if status --is-interactive
	function startnvidia --description "Switch to X server backed by the nvidia card"
		# sudo systemctl stop bumblebeed display-manager
		sudo modprobe -r bbswitch
		# set -x LD_LIBRARY_PATH "/usr/lib/nvidia:/usr/lib32/nvidia:/usr/lib:/usr/lib32"
		# startx -- -config xorg-nvidia.conf
		sudo systemctl start prime@$USER.service
	end

	function startintel --description "Start X server backed by the intel card"
		sudo systemctl start bumblebeed display-manager
	end

	function fish_user_key_bindings
		bind \cx "commandline | xsel --clipboard"
		bind \cv yank # paste from killring/clipboard
		bind -k ic yank # insert key
		bind \ev yank-pop
	end

	function erase_grep_options --on-variable GREP_OPTIONS --description "Delete GREP_OPTIONS if it is ever set"
		if set -q GREP_OPTIONS
			echo "SOMETHING TRIED TO SET GREP_OPTIONS to $GREP_OPTIONS!"
			set -e GREP_OPTIONS
		end
	end

	# Add path for completions/functions intended to go upstream
	set fish_function_path ~/.config/fish/test/functions $fish_function_path
	set fish_complete_path ~/.config/fish/test/completions $fish_complete_path

	set -g __fish_git_prompt_show_informative_status 0
	set -g __fish_git_prompt_showupstream informative
	set -g __fish_git_prompt_describe_style contains
	set -g __fish_git_prompt_showcolorhints 0

	if not set -q fish_initialized
		abbr -a alsamixer alsamixer -c0
		abbr -a e emacs -nw
		abbr -a mu4e emacs --eval "\(mu4e\)"
		abbr -a pm pulsemixer
		abbr -a rm rm -I
		abbr -a sc systemctl
		abbr -a upo upower -i /org/freedesktop/UPower/devices/battery_BAT0
		abbr -a usc systemctl --user
		# Double-escaping needed
		abbr -a d2 env WINEPREFIX=/home/alfa/.wine32/ wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Diablo\\ II/Diablo\\ II.exe
		set -U fish_initialized
	end

	if set -q SCRIPTHACK
		function fish_title; end
		function fish_right_prompt; end
		function fish_prompt; echo -n $PWD ">"; end
	end
end
