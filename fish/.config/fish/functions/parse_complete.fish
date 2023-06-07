function parse_complete
    # complete [( -c | --command | -p | --path )] COMMAND
    #               [( -c | --command | -p | --path ) COMMAND]...
    #               [( -e | --erase )]
    #               [( -s | --short-option ) SHORT_OPTION]...
    #               [( -l | --long-option | -o | --old-option ) LONG_OPTION]...
    #               [( -a | --arguments ) ARGUMENTS]
    #               [( -k | --keep-order )]
    #               [( -f | --no-files )]
    #               [( -F | --force-files )]
    #               [( -r | --require-parameter )]
    #               [( -x | --exclusive )]
    #               [( -w | --wraps ) WRAPPED_COMMAND]...
    #               [( -n | --condition ) CONDITION]
    #               [( -d | --description ) DESCRIPTION]

    argparse c/command= p/path= e/erase \
        s/short-option= l/long-option= o/old-option= \
        a/arguments= w/wraps= n/condition= d/description= \
        k/keep-order f/no-files F/force-files r/require-parameter x/exclusive \
        C/do-complete= \
        -- $argv 2>/dev/null
    or return

    if set -q _flag_d[1]
        echo $_flag_d
    end
end
