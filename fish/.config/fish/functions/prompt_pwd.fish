function prompt_pwd --description 'Print the current working directory, shortened to fit the prompt'
	set -q argv[1]; and switch $argv[1]
		case -h --help
			__fish_print_help prompt_pwd
			return 0
	end

	# This allows overriding fish_prompt_pwd_dir_length from the outside (global or universal) without leaking it
	set -q fish_prompt_pwd_dir_length; or set -l fish_prompt_pwd_dir_length 1
    set -q argv[1]; and set -l fish_prompt_pwd_dir_length $argv[1]

    set -l tmp
    set -l gitdir
    # Replace everything up to the vcs root with [rootname]
    # under the assumption that that is the name of the "project".
    # TODO: On my machine, it seems that the last two components are the safer bet, e.g.
    # kwin/tiling
    # exercism/cli (go)
    # elves/elvish (go)
    # new/exa-git (aur)
    # dev/fish-shell
    #
    # Either that, or a more robust system of abbreviations,
    # a non-hacky solution for which needs dictionaries.
    if set gitdir (command git rev-parse --show-toplevel 2>/dev/null)
        set tmp (string replace -- $gitdir '' $PWD)
        # Underline the "project name"
        # FIXME: The coloring here will leak, but I can't see a way to just undo underlining.
        set gitdir \[(set_color -u)(string replace -r '.*/([^/]+/)' '$1' -- $gitdir)(set_color normal; set_color $fish_color_cwd)\]
    else
	    # Replace $HOME with "~"
        # Only if we're not in a gitdir, otherwise $HOME/dev/$HOME would give weird results.
	    set realhome ~
        # FIXME: This will give weird results if any special regex chars are in $HOME,
        # but it should be a regex match because we only want it replaced at the beginning of the string.
	    set tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $PWD)
    end
    # The test will return false if $fish_... is non-numerical,
    # and the replace will not replace anything, so the PWD will be unshortened.
    # This is okay.
	if test $fish_prompt_pwd_dir_length = 0
		echo -s $gitdir $tmp
	else
		# Shorten to at most $fish_prompt_pwd_dir_length characters per directory
        # but only after the gitdir
		echo "$gitdir"(string replace -ar '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp)
	end
end
