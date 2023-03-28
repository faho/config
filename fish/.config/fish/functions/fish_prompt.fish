function fish_prompt
    set -l last_status $status

    set -l normal (set_color normal)
    set -l usercolor (set_color $fish_color_user)

    set -g __fish_git_prompt_showdirtystate 1
    # set -g __fish_git_prompt_showuntrackedfiles 1
    set -g __fish_git_prompt_showupstream informative
    set -g __fish_git_prompt_showcolorhints 1
    set -g __fish_git_prompt_use_informative_chars 1
    # Unfortunately this only works if we have a sensible locale
    string match -qi "*.utf-8" -- $LANG $LC_CTYPE $LC_ALL
    and set -g __fish_git_prompt_char_dirtystate \U1F4a9
    set -g __fish_git_prompt_char_untrackedfiles "?"

    set -l delim \U25BA
    # If we don't have unicode use a simpler delimiter
    string match -qi "*.utf-8" -- $LANG $LC_CTYPE $LC_ALL; or set delim ">"

    fish_is_root_user; and set delim "#"

    set -l cwd (set_color $fish_color_cwd)
    if command -sq cksum
        # randomized cwd color
        # We hash the physical PWD and turn that into a color. That means directories (usually) get different colors,
        # but every directory always gets the same color. It's deterministic.
        # We use cksum because 1. it's fast, 2. it's in POSIX, so it should be available everywhere.
        # set -l shas (math --base=hex "($(pwd -P | string escape --style=hex | string sub -s -5000 | string replace -ra '(...?.?.?.?)' '0x$1$1$1\n' | string join -n +)) % 0xffffff" | string sub -s 3 | string pad -c 0 -w 6 | string match -ra ..)
        set -l shas (pwd -P | cksum | string split -f1 ' ' | math --base=hex | string sub -s 3 | string pad -c 0 -w 6 | string match -ra ..)
        set -l col 0x$shas[1..3]

        # If the (simplified idea of) luminance is below 120 (out of 255), add some more.
        # (this runs at most twice because we add 60)
        set -l inds 1 2 3 1 2 3 1 2 3
        if test $col[1] -gt $col[2]
            test $col[1] -le $col[3]
            and set -e inds[1..2]
        else
            test $col[2] -le $col[3]
            and set -e inds[1]
            or set -e inds[1..2]
        end
        while test (math 0.2126 x $col[1] + 0.7152 x $col[2] + 0.0722 x $col[3]) -lt 120
            set col[$inds[1]] (math --base=hex "max 0, min(255, $col[$inds[1]] x 1.1 + 80)")
            set -e inds[1]
        end
        set -l col (string replace 0x '' $col | string pad -c 0 -w 2 | string join "")

        set cwd (set_color $col)
    end

    # Prompt status only if it's not 0
    set -l prompt_status
    test $last_status -ne 0; and set prompt_status (set_color $fish_color_error)"[$last_status]$normal"

    # Only show host if in SSH or container
    # Store this in a global variable because it's slow and unchanging
    if not set -q prompt_host
        set -g prompt_host ""
        if set -q SSH_TTY
            or begin
                command -sq systemd-detect-virt
                and systemd-detect-virt -q
            end
            set prompt_host $usercolor$USER$normal@(set_color $fish_color_host)$hostname$normal":"
        end
    end

    # Shorten pwd if prompt is too long
    set -l pwd
    # TODO: hg doesn't work here because we need that cdup,
    # or we'd run into problems with logical vs physical paths.
    if set -l vcs (git rev-parse --show-toplevel --show-prefix 2>/dev/null)
        if set -l pre (string replace -- $vcs[2] '' $PWD/); or test -z "$vcs[2]"
            set pwd (string join "" -- (prompt_pwd -d1 -D1 (string trim -rc / -- $pre)) \
                $normal(fish_vcs_prompt '[%s]') \
                $cwd/(prompt_pwd -- (string trim -rc / -- $vcs[2])))
        else
            set pwd (prompt_pwd) $normal(fish_vcs_prompt '(%s)')
        end
    else
        set pwd (prompt_pwd)
    end

    # echo -n -s $delim
    echo -n -s $prompt_host $cwd $pwd $normal $prompt_status $delim
end
