function gitcomp
    argparse h/head m/min= -- $argv
    or return

    set -l commits 0
    # (add "b" to the regex for betas)
    set -l tags (git -c versionsort.suffix=b tag --sort=v:refname --format="%(tag)"\t"%(creatordate:iso) (%(creatordate:relative))" | string match -r '^[0-9][0-9.]+\t.*$')
    set -ql _flag_h
    and set -a tags HEAD

    set -l max (git rev-list --count (string split -m1 -f1 \t -- $tags[-1]))
    for t in $tags
        set -l td (string split -m1 \t $t)
        set t $td[1]
        set -l newcommits (git rev-list --count $t)
        set -l perc (math -s 1 100 - $newcommits / $max x 100)
        if set -ql _flag_min
            and test $perc -gt $_flag_min
            set commits $newcommits
            continue
        end
        string join \t -- $t (math $newcommits - $commits) $perc% $td[2]
        set commits $newcommits
    end
end
