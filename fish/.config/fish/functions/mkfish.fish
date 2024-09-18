function mkfish --wraps=makepkg
    set -lx branch "#branch=master"
    set -l opts
    for arg in $argv
        switch $arg
            case "-*"
                set opts $opts $arg
            case "#*"
                set branch $arg
            case "*"
                set branch "#branch=$arg"
        end
    end
    makepkg -D ~/dev/build/fish-shell-git -sir $opts
end
