function fish_move
    set -q argv[3]
    or return 1
    set -l regex $argv[1]
    set -l replacement $argv[2]
    set -e argv[1..2]

    for f in $argv
        set -l new (string replace -rf $regex $replacement -- $f | string collect)
        or continue
        mv -- $f $new
    end
end
