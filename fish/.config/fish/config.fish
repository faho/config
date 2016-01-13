#
# Init file for fish
#

set -gx LANG de_DE.UTF-8
set -gx LANGUAGE de
set -gx GPGKEY 36EBBDB3
set -gx GPG_TTY (tty)
set -gx GOPATH ~/dev/go
set -U  fish_user_paths ~alfa/.local/bin $GOPATH/bin
set -gx MAIL $HOME/.mail
set -gx MAILDIR $MAIL
set -gx CDPATH . $HOME $HOME/Videos
set -gx WINEDEBUG "-all"
set -gx NO_AT_BRIDGE 1
set -gx EDITOR "emacs -nw"
set -gx MPD_HOST "/run/user/1000/mpd.socket"
set -gx SWT_GTK3 0
set -gx _JAVA_OPTIONS "-Dawt.useSystemAAFontsettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"
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
	switch $TERM
		case "konsole*" "xterm*"
			set -gx LS_COLORS 'rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'
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
