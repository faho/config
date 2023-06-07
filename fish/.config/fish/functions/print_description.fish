# Defined via `source`
function print_description
    argparse l/len= n/names-only -- $argv
    or return
    set -l len $_flag_len 100
    set -l names_only $_flag_names_only
    for file in $argv
        set -l lines (string match -re '^\s*complete ' < $file | while read -lat f
            argparse c/command= p/path= e/erase \
                s/short-option= l/long-option= o/old-option= \
                a/arguments= w/wraps= n/condition= d/description= \
                k/keep-order f/no-files F/force-files r/require-parameter x/exclusive \
                C/do-complete= \
                -- $f 2>/dev/null
            or continue

            if set -q _flag_d[1]
                echo $_flag_d
            end
        end | sort -u | string match -r '^.{'$len[1]',}$')
        and echo (set_color blue)$file(set_color normal)
        and not set -q names_only[1]
        and printf %s\n $lines
    end
end
