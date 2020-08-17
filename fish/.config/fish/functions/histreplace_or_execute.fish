# Defined in /tmp/fish.qpmL3V/histreplace_or_execute.fish @ line 2
function histreplace_or_execute
    set -l oldcmd (commandline)
    set -l newcmd
    set -l modified
    for line in $oldcmd
        # HACK: EVIL fix for multiline-commandlines
        # (we can't use commandline --tokenize because that
        #  removes the differences between lines)
        # This would really need a tokenize builtin
        commandline -r -- $line
        set -l cmdline (commandline -o)
        set -l newcmdline
        for token in $cmdline
            if string match -q '!!' -- $token
                set token $history[1]
                set modified true
            end
            set newcmdline (string join " " -- $newcmdline $token)
        end
        set newcmd $newcmd $newcmdline
    end
    if set -q modified[1]
        commandline -r -- $newcmd
    else
        commandline -r -- $oldcmd
        commandline -f execute
    end
end
