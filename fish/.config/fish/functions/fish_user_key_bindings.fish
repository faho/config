function fish_user_key_bindings
	fzf_key_bindings
    # In paste-mode, self-insert except for the sequence to get out of it
    # This should return to the previous mode
    bind -M paste -m default \e\[201~ force-repaint
    bind -M paste "" self-insert
    # Without this, a \r will overwrite the other text, rendering it invisible - which makes the exercise kinda pointless.
    bind -M paste \r "commandline -i \n"
    bind -M default -m paste \e\[200~ ''
    # Enable bracketed paste mode
    printf "\e[?2004h"
end
