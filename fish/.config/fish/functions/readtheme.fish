function readtheme
    set -l file $argv[1]
    set -l dir ~/dev/fish-shell/share/tools/web_config/themes

    if not test -e "$dir/$file.theme"
        echo "No such theme: $file" >&2
        return 1
    end

    set -l have_color 0
    while read -la toks
        set toks (string trim -- $toks)
        set toks (string trim -c "'\"" -- $toks)

        # We only allow color variables.
        # Not the specific list, but something named *like* a color variable.
        #
        # This also takes care of empty lines and comment lines.
        string match -rq '^fish_(?:pager_)?color.*$' -- $toks[1]
        or continue

        set -g $toks
        set have_color 1
    end < $dir/$file.theme

    # Return true if we changed at least one color
    test $have_color -eq 1
end
