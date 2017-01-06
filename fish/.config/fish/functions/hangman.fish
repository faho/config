function hangman
	if not test -r /usr/share/dict/words
		echo "Please install a wordlist in /usr/share/dict/words"
		return 1
	end
	set -l usedchars
	set -l word
	# Match \p{L&} because that includes utf-8 letters like "ö"
	set -l legal '^\p{L&}+$'
	# With a proper word list this should succeed immediately
    # set -l w (string match -r $legal < /usr/share/dict/words)
    # while read -l www; set w $w $www; end < /usr/share/dict/words
	while set -l w (shuf -n 1 /usr/share/dict/words)
		if string match -qr $legal $w
			set word $w
			break
		end
	end
    set word (random choice $w)
	set word (string split -- "" $word)
	set -l restword $word # if we'd like to keep the word
	set -l solved (string replace -r -- '.' '_' $word)
	set -l wrong
	while not string match -qr '^\t+$' (string join "" $restword)
		clear
		echo -s (set_color brgreen) "$solved"
		echo -s (set_color brblue) "Used characters: " (set_color normal) "$usedchars"
		echo -s (set_color brred) "Characters not included: " (set_color normal) "$lost" " (" (count $lost) "/" (count $restword) ")"
		read -n 1 -p 'echo "Character:"' -l char

		# Reject invalid or already used chars
		contains -- $char $usedchars $lost; and continue
		string match -qr $legal $char; or continue

		# HACK: This is a tiny bit ugly
		if string match -qi -- $char $restword
			while set -l ind (contains -i -- $char $restword)
				set solved[$ind] $char
				set restword[$ind] \t
			end
			set usedchars $usedchars $char
			# HACK: This has the side-effect of showing the solved word at the end
			echo -s (set_color green) "$solved"
		else
			set lost $lost $char
		end
		# Quite a simple lose-condition
		if test (count $lost) -gt (count $restword)
			echo -s (set_color red) "LOST"
			echo -s "Word was: " (string join "" $word)
			return 1
		end
	end
	echo "WON"
end
