#
# Init file for fish
#

# These go first, because other stuff depends on them.
set -gx XDG_CACHE_HOME $HOME/.cache
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_DATA_HOME $HOME/.local/share
set -gx --path XDG_DATA_DIRS $XDG_DATA_HOME/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share

set -gx ANDROID_HOME ~/.android # /opt/android-sdk
set -gx ASPROOT ~/packages/asp
set -gx CDPATH . ~ (test -e ~/Videos; and echo ~/Videos)
set -gx EDITOR emacs -nw
set -gx GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
set -gx GOPATH ~/dev/go
set -gx GTK2_RC_FILES "$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
set -gx LESSHISTFILE -
# set -gx MAIL ~/.mail
# set -gx MAILDIR $MAIL
set -gx MICRO_TRUECOLOR $fish_term24bit
set -gx MPD_HOST "$XDG_RUNTIME_DIR/mpd.socket"
set -gx NVIM_TUI_ENABLE_CURSOR_SHAPE 1
set -gx NVIM_TUI_ENABLE_TRUE_COLOR $fish_term24bit
set -gx NO_AT_BRIDGE 1
set -gx PAGER less
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

if not set -q fish_user_paths
    set -U fish_user_paths ~/.local/bin
end

if status --is-interactive
    set -gx GPGKEY 36EBBDB3
    set -gx GPG_TTY (tty)

    function startintel --description "Start X server backed by the intel card"
        sudo systemctl start bumblebeed display-manager
    end

    # Add path for completions/functions intended to go upstream
    set fish_function_path ~/.config/fish/test/functions $fish_function_path
    set fish_complete_path ~/.config/fish/test/completions $fish_complete_path

    set fish_prompt_pwd_dir_length 0
    set -g fish_cursor_insert line

    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_showuntrackedfiles 1
    set -g __fish_git_prompt_showupstream informative
    set -g __fish_git_prompt_showcolorhints 1
    set -g __fish_git_prompt_use_informative_chars 1
    set -g fish_cursor_insert line
    set -g fish_cursor_default block
    set -g fish_cursor_visual underscore
end
