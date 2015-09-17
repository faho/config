function duthis --wraps "du"
    if [ -z $argv ]
        set argv ./* ./.*
    end
    du -sch $argv | sort -h
end
