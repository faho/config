# Defined in /tmp/fish.hhfncX/fish_clear.fish @ line 1
function fish_clear --description 'Clear the terminal'
    # clear from ncurses >= 6.0 also sends the "E3" sequence that clears scrollback
    # Even `tput clear` does it.
    # So instead we figure out just the "clear" sequence.
    # (It might be possible to just always `printf \e\[H\e\[2J`, but let's be pedantic here)
    set -l clear (infocmp | string match -r 'clear=[^,]+,' | string replace -r 'clear=(.+),' '$1')
    if test -n "$clear"
        printf (string replace -a "E" "e" -- $clear)
    end
end
