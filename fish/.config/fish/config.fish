#
# Init file for fish
#

# These go first, because other stuff depends on them.
set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_DATA_HOME "$HOME/.local/share"

set -gx ANDROID_HOME ~/.android # /opt/android-sdk
set -gx ASPROOT ~/packages/asp
set -gx CDPATH . ~ (test -e ~/Videos; and echo ~/Videos)
set -gx EDITOR emacs -nw
set -gx GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
set -gx GOPATH ~/dev/go
set -gx GTK2_RC_FILES "$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
set -gx LESSHISTFILE "-"
# set -gx MAIL ~/.mail
# set -gx MAILDIR $MAIL
set -gx MICRO_TRUECOLOR $fish_term24bit
set -gx MPD_HOST "$XDG_RUNTIME_DIR/mpd.socket"
set -gx NVIM_TUI_ENABLE_CURSOR_SHAPE 1
set -gx NVIM_TUI_ENABLE_TRUE_COLOR $fish_term24bit
set -gx NO_AT_BRIDGE 1
set -gx PAGER "less"
set -gx PRIMUS_SYNC 1
# How GNU ls displays weird characters in filenames
# This one displays "a\tb" and "a\ b" (without the quotes)
# The default in new GNU ls displays bashy "'a'$'\t''b'".
# NOTE: This enables it _even when output isn't a tty_,
# so it's worse than useless.
# set -gx QUOTING_STYLE escape
set -gx SSH_ASKPASS /usr/bin/ksshaskpass
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
set -gx SWT_GTK3 0
set -gx WINEDEBUG "-all,fixme-all"

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

    end
end
