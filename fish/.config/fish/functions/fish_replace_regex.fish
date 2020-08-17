function fish_replace_regex
    set -l cmd (commandline)
    function __fish_replace_old -V cmd --on-signal SIGINT
        functions -e __fish_replace_old
        commandline "$cmd"
        commandline -f repaint
    end
    read -l match -p 'echo Match:'
    read -l replacement -p 'echo Replacement:'
    echo \t (string replace -ar -- '('$match')' (set_color red)'$1'(set_color green)"$replacement"(set_color normal) $cmd)
    set -l new (string replace -ar -- '('$match')' $replacement $cmd)
    read -l yn -p 'echo "Accept?"'
    functions -e __fish_replace_old
    if test "$yn" = y -o "$yn" = Y -o "$yn" = J -o "$yn" = j
        commandline "$new"
    else
        commandline "$cmd"
    end
end
