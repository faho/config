function try
	argparse -n try -s -x s,f s/status= f/fail o/output= -- $argv
or return
set -l retval 0
set -q _flag_status; or set -l _flag_status 0
set -l output ($argv)
set -l stat $status
if not test $_flag_status -eq $stat
echo "Wrong status $stat, expected $_flag_status" >&2
    set retval 1
end

if set -q _flag_output; and test "$output" != "$_flag_output"
echo "Wrong output" >&2
set retval (math $retval + 2)
end
return $retval
end
