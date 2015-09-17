function show_colors
	set -l hex {0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F}
	for color in $hex$hex$hex
		echo -n (set_color $color)$color
	end
end
