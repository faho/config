# Defined in /tmp/fish.mREV91/forget.fish @ line 2
function forget
	history delete --case-sensitive --exact $history[1]
end
