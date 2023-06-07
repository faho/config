function fish_replace_regex
    set -l cmd (commandline)
    function __fish_replace_old -V cmd --on-signal SIGINT
        functions -e __fish_replace_old
        commandline "$cmd"
        commandline -f repaint
    end
    echo
    read -l match -P 'Match: '
    or begin
        functions -e __fish_replace_old
        commandline -f repaint
        return
    end
    read -l replacement -P 'Replacement: '
    or begin
        functions -e __fish_replace_old
        commandline -f repaint
        return
    end
    echo \t (string replace -ar -- '('$match')' (set_color --underline 444444)'$1'(set_color green)"$replacement"(set_color normal) $cmd)
    set -l new (string replace -ar -- '('$match')' $replacement $cmd)
    read -l yn -P 'Accept (y/N)? ' -n 1
    or begin
        functions -e __fish_replace_old
        commandline -f repaint
        return
    end
    functions -e __fish_replace_old
    if test "$yn" = y -o "$yn" = Y -o "$yn" = J -o "$yn" = j
        commandline "$new"
    else
        commandline "$cmd"
    end
end
