function fish_right_prompt
	if git rev-parse ^ /dev/null
        # Purple if branch detached else green
        git branch -qv ^/dev/null | grep "\*" | grep -q detached
            and set_color purple --bold
            or set_color green --bold

        # Need optimization on this block (eliminate space)
        git name-rev --name-only HEAD ^/dev/null

        # Merging state
        git merge -q ^ /dev/null; or printf ':'(set_color red)'merge'
        printf ' '

        # Symbols
		for i in (git branch -qv --no-color ^/dev/null |grep \*|cut -d' ' -f4-|cut -d] -f1|tr , \n)\
            (git status --porcelain ^/dev/null  | cut -c 1-2 | uniq)
			switch $i
                case "*[ahead *"
                    printf (set_color purple)⬆' '
                case "*behind *"
                    printf (set_color purple)⬇' '
				case "."
					printf (set_color green)✚' '
				case " D"
					printf (set_color red)✖' '
				case "*M*"
					printf (set_color blue)✱' '
                case "*R*"
                    printf (set_color purple)➜' '
                case "*U*"
                    printf (set_color brown)═' '
				case "??"
					printf (set_color white)◼' '
			end
		end
	end
	# set -l battery_percentage (upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage | cut -d":" -f2 | cut -d"%" -f1)
	# set -l battery (bar $battery_percentage)
	set -l bat (battery)
	set -l plug
	set -q BATTERY_IS_PLUGGED; and set plug "⚡"
	set -l d (date "+%R %A %F")
	printf "%s%s %s" $plug $bat $d
end
