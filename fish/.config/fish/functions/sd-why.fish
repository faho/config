function sd-why
	for unit in $argv
		set -l reason (systemctl show $unit | string match "*By=*")
		echo (set_color blue)"$unit"(set_color normal)": $reason"
	end
end
