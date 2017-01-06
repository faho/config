#
# Init file for fish
#

set -gx ASPROOT ~/packages/asp
set -gx CDPATH . ~ (test -e ~/Videos; and echo ~/Videos)
set -gx EDITOR "emacs -nw"
set -gx GOPATH ~/dev/go
set -gx GTK2_RC_FILES "$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
set -gx LESSHISTFILE "-"
set -gx MAIL ~/.mail
set -gx MAILDIR $MAIL
set -gx MICRO_TRUECOLOR $fish_term24bit
set -gx MPD_HOST "/run/user/1000/mpd.socket"
set -gx NO_AT_BRIDGE 1
set -gx PAGER "less"
set -gx PRIMUS_SYNC 1
set -gx SSH_ASKPASS /usr/bin/ksshaskpass
set -gx SSH_AUTH_SOCK "$XDG_CONFIG_HOME/gnupg/S.gpg-agent.ssh"
set -gx SWT_GTK3 0
set -gx WINEDEBUG "-all"
set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_DATA_HOME "$HOME/.local/share"

# SDL mapping for PS3 controller
# set -x SDL_GAMECONTROLLERCONFIG "030000004c0500006802000011010000,PS3 Controller,a:b14,b:b13,back:b0,dpdown:b6,dpleft:b7,dpright:b5,dpup:b4,guide:b16,leftshoulder:b10,leftstick:b1,lefttrigger:b8,leftx:a0,lefty:a1,rightshoulder:b11,rightstick:b2,righttrigger:b9,rightx:a2,righty:a3,start:b3,x:b15,y:b12,platform:Linux,"

if status --is-interactive
    set -gx GPGKEY 36EBBDB3
    set -gx GPG_TTY (tty)
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

    # Add path for completions/functions intended to go upstream
    set fish_function_path ~/.config/fish/test/functions $fish_function_path
    set fish_complete_path ~/.config/fish/test/completions $fish_complete_path
    # Make fish < 2.3.0 work a bit nicer with this config
    if not type -q string
        function fish_prompt
            echo (prompt_pwd)">"
        end
        function fish_right_prompt
        end
    end
    type -q fzf_key_bindings
    and fzf_key_bindings
end
