#
# Init file for fish
#

# The greeting is annoying and useless
set -e fish_greeting
set -x GPGKEY 36EBBDB3
set -x GPG_TTY (tty)
set -x PATH $PATH $HOME/.local/bin
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
set -x SSH_AUTH_SOCK "$HOME/.gnupg/S.gpg-agent.ssh"

# SDL mapping for PS3 controller
set -x SDL_GAMECONTROLLERCONFIG "030000004c0500006802000011010000,PS3 Controller,a:b14,b:b13,back:b0,dpdown:b6,dpleft:b7,dpright:b5,dpup:b4,guide:b16,leftshoulder:b10,leftstick:b1,lefttrigger:b8,leftx:a0,lefty:a1,rightshoulder:b11,rightstick:b2,righttrigger:b9,rightx:a2,righty:a3,start:b3,x:b15,y:b12,platform:Linux,"

# XDG
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_DATA_HOME "$HOME/.local/share"
set -x XDG_CACHE_HOME "$HOME/.cache"

function duthis --wraps "sort"
	if [ -z $argv ]
		set argv ./* ./.*
	end
	du -sch $argv  | sort -h
end

function makepkgs --description "Build multiple packages with makepkg"
	if [ (count $argv) -eq 0 ]
		echo "Please specify packages"
		return
	end
	set -l built 0  # has a new package been built last run? (0=true, 1=false)
	while [ $built -eq 0 -a (count $argv) -gt 0 ]
		set -l built 1
		for pkg in $argv
			echo "Making $pkg"
			pushd $pkg
			# Install dependencies and the resulting package, but only when needed
			# Don't remove dependencies because they might be needed by the next package
			makepkg -si --needed
			if [ $status -eq 0 ]
				# Remove package that has been built successfully
				set -e argv[(contains -i $pkg $argv)]
				set built 0
			end
			popd
		end
	end
	if [ $built -gt 0 ]
		echo "No new packages could be built, please check"
	end
end

function rm --wraps "rm"
	command rm -I $argv
end

function alsamixer --wraps "alsamixer"
	command alsamixer -c0
end

alias mu4e="emacs --eval '(mu4e)'"
alias upo="upower -i /org/freedesktop/UPower/devices/battery_BAT0"
abbr -a sc=systemctl
abbr -a usc="systemctl --user"
alias abs="$HOME/dev/abs-replacement/abs.sh"

# Execute su via sudo if there are no arguments
# This results in ~/.config/fish/fishd* not being overwritten
function su
	if count $argv > /dev/null
		command su $argv
	else
		sudo su
	end
end

function startnvidia
	# sudo systemctl stop bumblebeed display-manager
	# sudo modprobe -r bbswitch
	# set -x LD_LIBRARY_PATH "/usr/lib/nvidia:/usr/lib32/nvidia:/usr/lib:/usr/lib32"
	# startx -- -config xorg-nvidia.conf
	sudo systemctl start prime@$USER.service
end

function startintel
	sudo systemctl start bumblebeed display-manager
end
