function duthis
    set -q argv[1]; or set argv * .*
    du -sch $argv | sort -h
end
