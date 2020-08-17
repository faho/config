function snippets
    set -l configdir ~/.config

    if set -q XDG_CONFIG_HOME
        set configdir $XDG_CONFIG_HOME
    end
    test -f $__fish_datadir/__fish_build_paths.fish; and source $__fish_datadir/__fish_build_paths.fish
    set -l sourcelist
    set -l dirs $configdir/fish/conf.d $__fish_sysconfdir/conf.d $__extra_confdir
    while set -q dirs[1]
        for file in $dirs[1]/*.fish
            set -l basename (string replace -r '^.*/' '' -- $file)
            contains -- $basename $sourcelist; and continue
            echo -n "$file"
            if [ -f $file -a -r $file ]
                echo " (sourced)"
            else
                echo " (not sourced)"
            end
            if set -q dirs[2]
                for f in $dirs[2..-1]/$basename
                    test -e $f; and echo \t"masks $f"
                end
            end
            set sourcelist $sourcelist $basename
        end
        set -e dirs[1]
    end
    return 0
end
