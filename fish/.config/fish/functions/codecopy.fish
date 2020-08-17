# Defined in /tmp/fish.MJ1GA4/codecopy.fish @ line 2
function codecopy --description 'Copy stdin to clipboard as a markdown codeblock'
    set -l lines
    while read -l line
        set line (string replace -r '^' '    ' -- $line)
        set lines $lines $line
    end
    printf '%s\n' $lines | xsel --clipboard
end
