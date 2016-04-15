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
	function erase_grep_options --on-variable GREP_OPTIONS --description "Delete GREP_OPTIONS if it is ever set"
		if set -q GREP_OPTIONS
			echo "SOMETHING TRIED TO SET GREP_OPTIONS to $GREP_OPTIONS!"
			set -e GREP_OPTIONS
		end
	end
end
