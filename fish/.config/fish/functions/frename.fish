function frename
    argparse --min-args=2 -is do -- $argv
    or return
    set -l regex $argv[1]
    set -l replacement $argv[2]

    set -l col (set_color normal) (set_color brred)

    set -l olds
    set -l news
    set -l have_overlap 0
    set -e argv[1..2]

    set -q argv[1]
    or set argv *
    
    while set -q argv[1]
        set -l f $argv[1]
        set -e argv[1]
        set -l new "$(string replace -r -- $regex $replacement $f)"
        or continue

        if set -l ind (contains -i -- $new $olds); or set -l ind (contains -i -- $new $news)
            printf '%s%s%s: \'%s\' %s->%s \'%s\'\n' $col[2] Overlap $col[1] $f $col[2] $col[1] $new >&2
            printf '%s%s%s: \'%s\'\n' $col[2] "Other file" $col[1] $olds[$ind] >&2
            set have_overlap 1
        end

        set -a olds $f
        set -a news $new
    end

    if test "$have_overlap" = 1
        echo Refusing because of overlap >&2
        return 2
    end

    for ind in (seq (count $olds))
        echo "($ind)" mv -- \'(set_color brgreen)$olds[$ind]$col[1]\' \'(set_color brblue)$news[$ind]$col[1]\'
    end

    if not set -q _flag_do[1]
        if isatty stdout; and isatty stdin
            set -l cont 0
            set -l nums
            while test $cont -eq 0
                while read -l task -P"What do? (e)dit, (m)ove, (c)ancel?"
                    for ind in (seq (count $olds))
                        echo "($ind)" mv -- \'(set_color brgreen)$olds[$ind]$col[1]\' \'(set_color brblue)$news[$ind]$col[1]\'
                    end

                    switch $task
                        case m
                            set cont 1
                            break
                        case c
                            return 1
                        case e
                            set -l tmp (mktemp)
                            printf '%s\n' $news >$tmp
                            if $EDITOR $tmp
                                set -l newnew (cat $tmp)
                                set -S newnew
                                rm $tmp
                                if test (count $newnew) -ne (count $news)
                                    echo "Please don't add or delete lines" >&2
                                    continue
                                end

                                for ind in (seq (count $news) -1 1)
                                    if string match -q '#*' -- $newnew[$ind]
                                        set -e news[$ind]
                                        set -e olds[$ind]
                                    else
                                        set news[$ind] $newnew[$ind]
                                    end
                                end
                                break
                            end
                            rm $tmp
                        case '*'
                            if string match -rq '[\d]([\d]+,)*' -- $task
                                set nums (string split , $task)
                                break
                            end
                            continue
                    end
                end
                for num in $nums
                    read -l ednum -P"$(set_color brgreen)$olds[$num]$(set_color normal) -> " -c "$news[$num]"
                    set news[$num] $ednum
                end

                for ind in (seq (count $olds))
                    echo "($ind)" mv -- \'(set_color brgreen)$olds[$ind]$col[1]\' \'(set_color brblue)$news[$ind]$col[1]\'
                end
            end
        end
        return 1
    end

    if set -q _flag_do[1]
        for ind in (seq (count $olds))
            mv -- $olds[$ind] $news[$ind]
        end
    end
end
