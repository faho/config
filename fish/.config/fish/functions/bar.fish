function bar --description "Display percentage bars"
	for num in $argv
		test $num -ge 88; and printf "█"; and continue
		test $num -ge 76; and printf "▊"; and continue
		test $num -ge 64; and printf "▋"; and continue
		test $num -ge 50; and printf "▌"; and continue
		test $num -ge 38; and printf "▍"; and continue
		test $num -ge 26; and printf "▎"; and continue
		test $num -ge 14; and printf "▏"; and continue
		test $num -lt 14; and printf " "; and continue
	end
end
