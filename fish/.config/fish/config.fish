#
# Init file for fish
#

set -x GPGKEY 36EBBDB3
set -x GPG_TTY (tty)
set -U fish_user_paths ~alfa/.local/bin
set -x MAIL $HOME/.mail
set -x MAILDIR $MAIL
set -x CDPATH . $HOME $HOME/Videos
set -x WINEDEBUG "-all"
set -x NO_AT_BRIDGE 1
set -x EDITOR "emacs -nw"
set -x MPD_HOST "/run/user/1000/mpd.socket"
set -x SWT_GTK3 0
set -x _JAVA_OPTIONS "-Dawt.useSystemAAFontsettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"
set -x PAGER "less"

# SDL mapping for PS3 controller
set -x SDL_GAMECONTROLLERCONFIG "030000004c0500006802000011010000,PS3 Controller,a:b14,b:b13,back:b0,dpdown:b6,dpleft:b7,dpright:b5,dpup:b4,guide:b16,leftshoulder:b10,leftstick:b1,lefttrigger:b8,leftx:a0,lefty:a1,rightshoulder:b11,rightstick:b2,righttrigger:b9,rightx:a2,righty:a3,start:b3,x:b15,y:b12,platform:Linux,"

# XDG
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_DATA_HOME "$HOME/.local/share"
set -x XDG_CACHE_HOME "$HOME/.cache"

set -x SSH_AUTH_SOCK "$XDG_CONFIG_HOME/gnupg/S.gpg-agent.ssh"
set -x LESSHISTFILE "-"

set FISH_CLIPBOARD_CMD "cat"

if status --is-interactive
    logo
	function rm --wraps "rm"
		command rm -I $argv
	end

	function alsamixer --wraps "alsamixer"
		command alsamixer -c0
	end

	abbr -a sc=systemctl
	abbr -a usc="systemctl --user"
	abbr -a pm="pulsemixer"

	#alias mu4e emacs --eval "'(mu4e)'"
	function mu4e
		emacs --eval "(mu4e)" $argv
	end

	#alias abs $HOME/dev/abs-replacement/abs.sh
	function abs
		~/dev/abs-replacement/abs.sh $argv
	end

	#alias upo upower -i /org/freedesktop/UPower/devices/battery_BAT0
	function upo
		upower -i /org/freedesktop/UPower/devices/battery_BAT0 $argv
	end

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

	set -g fish_term24bit 1

	# Add path for completions/functions intended to go upstream
	set fish_function_path ~/.config/fish/test/functions $fish_function_path
	set fish_complete_path ~/.config/fish/test/completions $fish_complete_path

	set -g __fish_git_prompt_show_informative_status 0
	set -g __fish_git_prompt_showupstream informative
	set -g __fish_git_prompt_describe_style contains
	set -g __fish_git_prompt_showcolorhints 0

	# Assorted emacs ansi-term workarounds:
	if string match -i "eterm*" -- $TERM >/dev/null
		function fish_title; end
	end
end
