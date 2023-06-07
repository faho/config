function rainbow
    set -l colors (set_color --print-colors | string match -rv black\|white\|normal )
    set -l ind 0
    for arg in $argv
        for let in (string split '' -- $arg)
            set ind (math $ind % (count $colors) + 1)
            set_color $colors[$ind]
            echo -n $let
        end
        echo
    end
end
