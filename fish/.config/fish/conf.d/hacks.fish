if status --is-interactive
	switch $TERM
		case "eterm*"
			function fish_title; true; end
	end
	if set -q SCRIPTHACK
		function fish_title; end
		function fish_right_prompt; end
		function fish_prompt; echo -n $PWD ">"; end
	end
end
