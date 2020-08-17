function rename_watch
    for file in Watch-*.mp4
        set -l series (string replace -r 'Watch-(.*)-Online--(.*)--S(.*).mp4' '$1' -- $file | string replace -a -- '-' ' ')
        set -l number (string replace -r '.*(S\d+E\d+).*.mp4' '$1' -- $file)
        set -l name (string replace -r 'Watch-.*-Online--(.*)--S\d+E.*.mp4' '$1' -- $file | string replace -a -- '-' ' ')
        set -l newname "$series $number - $name.mp4"
        mv $file $newname
    end
end
