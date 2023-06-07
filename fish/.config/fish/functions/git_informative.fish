# Defined in /tmp/fish.1VnhbZ/git_informative.fish @ line 2
function git_informative
    if test "$__fish_git_prompt_showdirtystate" -eq 1 2>/dev/null
        echo NOT INFORMATIVE
        set -e __fish_git_prompt_showdirtystate
        set -e __fish_git_prompt_showuntrackedfiles
        set -e __fish_git_prompt_showupstream
    else
        echo INFORMATIVE
        set -g __fish_git_prompt_showdirtystate 1
        set -g __fish_git_prompt_showuntrackedfiles 1
        set -g __fish_git_prompt_showupstream informative
    end
end
