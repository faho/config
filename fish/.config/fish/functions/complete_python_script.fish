function complete_python_script
    for t in (commandline -opc)[2..-1]
        if string match -qv -- '-*' -- $t
            echo -- testing $t >&2
            if test -f "$t" -a -x "$t"
                echo Completing $t >&2
                complete -C"$t "
            end
            return
        end
    end
end
