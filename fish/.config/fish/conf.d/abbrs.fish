if not status is-interactive
    exit
end

abbr -a -- alsamixer alsamixer -c0
abbr -a -- e emacs -nw
abbr -a -- \$PAGER less
# abbr -a -- mu4e emacs --eval "\(mu4e\)"
abbr -a -- pm pulsemixer
abbr -a -- rm rm -I
abbr -a -- sc systemctl
abbr -a -- upo upower -i /org/freedesktop/UPower/devices/battery_BAT0
abbr -a -- usc systemctl --user
# Double-escaping needed
abbr -a -- d2 'env WINEPREFIX=/home/alfa/.wine32/ wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Diablo\\ II/Diablo\\ II.exe'
# curl requires one "-O" per argument, but doesn't complain about getting too many.
abbr -a -- c curl -LOOOOOOO -C -

if builtin -q abbr
    function autocd
        set -q argv[1]
        or return 1

        type -q "$argv[1]"
        and return 1
        
        echo -- $argv[1] | read -lt dirs
        string match -rq '^(.+)?/*'
        or set -p dirs $CDPATH/$argv

        path is -dx -- $dirs
        or return 1

        echo -- cd $argv[1]
    end

    abbr -a --regex '.*/' --function autocd autocd

    function multicd; echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../); end

    abbr --add dotdot --regex '^\.\.+$' --function multicd

    function dir_shortcut
        set -l key (string replace -r '^D~' '' -- $argv[1])
        or return
        set -l var dirs_$key
        set -q $var
        or return
        echo -- $$var
    end

    abbr -a dirshortcuts --position anywhere --regex '^D~.*' --function dir_shortcut

    set -g dirs_fish ~/dev/fish-shell
    function commandpath
        set -l cmd (string replace -r '^=(.+)$' '$1' -- $argv)
        or return
        command -s $cmd
    end
    abbr --add --position anywhere --regex '=.*' --function commandpath commandpath

    # function histreplace
    #     set -l entrynum 1
    #     set -l args 1..-1
    #     string match -q '*^' -- $argv
    #     and set args 2
    #     string match -q '*$' -- $argv
    #     and set args -1
    #     string match -q '*\*' -- $argv
    #     and set args 2..-1
    #     # TODO: `!num:beg-end` to select tokens by number, with range (0-based)

    #     if set -l num (string replace -rf '!(-?[0-9]+).*' '$1' -- $argv)
    #         set entrynum (math -1 x $num)
    #     end

    #     set -l toks
    #     printf %s $history[$entrynum] | while read -lat tokens
    #         set -a toks $tokens
    #     end

    #     echo -- $toks[$args]
    # end

    # abbr --add histreplace --function histreplace --position anywhere --regex '!.*'

    function histreplace
        switch "$argv[1]"
            case !!
                echo -- $history[1]
                return 0
            case '!$'
                echo -- $history[1] | read -lat tokens
                echo -- $tokens[-1]
                return 0
        end
        return 1
    end

    abbr --add !! --function histreplace --position anywhere
    abbr --add '!$' --function histreplace --position anywhere

    function cless
        # Abbr. Try to add a "--color=always" option and pipe to less.
        # If we don't recognize the command, we still pipe to less.
        # (alternative is to write an empty string and return 0 to eliminate the abbr key)
        set -l tok (commandline -opc)
        # Remove the abbr itself
        set -e tok[-1]
        while contains -- $tok[1] and or not command builtin
            set -e tok[1]
        end

        if not set -q tok[1]
            echo %
            return
        end
        
        set -l opt
        switch "$tok[1]"
            case grep diff ls
                # TODO: Check for gnu version
                set opt --color=always
            case jq
                set opt -C
            case g++{,'-*'} gcc{,-'*'}
                set opt -fdiagnostics-color=always
            case git
                # it's git, we need to figure out the subcommand
                set -l optspecs version h/help C= c=+ 'e-exec-path=?' html-path man-path info-path p/paginate \
                    P/no-pager no-replace-objects bare git-dir= work-tree= namespace= super-prefix= \
                    literal-pathspecs glob-pathspecs noglob-pathspecs icase-pathspecs
                if not argparse -s $optspecs -- $tok[2..] 2>/dev/null
                    set opt
                # at least log and grep won't use options after the first nonopt
                # so we just give up parsing it
                else if set -q argv[2]
                    and contains -- $argv[1] log grep
                    set opt
                else
                    set opt --color=always
                end
            case '*'
                set opt
        end

        # -- argument, can't give an option
        if contains -- -- $tok
            set opt
        end

        set -l popt
        set -l pager less
        set -q PAGER
        and echo -- $PAGER | read -lat pager
        if not type -q $pager[1]
            echo %
            return
        end

        switch $pager[1]
            case less
                set popt -R --quit-if-one-screen
            case lv
                set popt -c
        end

        # We print our color option, then the cursor position,
        # and then the pager - even if we don't otherwise recognize the command
        echo -- $opt % "| $pager $popt"
    end

    abbr --add cless --function cless --set-cursor --position anywhere

    # Defined via `source`
    function replace_pf
        echo '(path filter % -- '(string replace -r '~pf$' '' -- $argv[1])')'
    end
    abbr --add pf --regex '.+~pf' --function replace_pf --set-cursor --position anywhere
end
