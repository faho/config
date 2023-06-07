function hashcolor
set -l shas (echo -- $argv | cksum | string split -f1 ' ' | math --base=hex | string sub -s 3 | string pad -c 0 -w 6 | string match -ra ..)
set -l col 0x$shas[1..3]

# If the (simplified idea of) luminance is below 120 (out of 255), add some more.
# (this runs at most twice because we add 60)
set -l inds 1 2 3 1 2 3 1 2 3
if test $col[1] -gt $col[2]
test $col[1] -le $col[3]
and set -e inds[1..2]
else
test $col[2] -le $col[3]
and set -e inds[1]
or set -e inds[1..2]
end
while test (math 0.2126 x $col[1] + 0.7152 x $col[2] + 0.0722 x $col[3]) -lt 100
set col[$inds[1]] (math --base=hex "min(255, $col[$inds[1]] x 1.1 + 40)")
set -e inds[1]
end
set -l col (string replace 0x '' $col | string pad -c 0 -w 2 | string join "")

set cwd (set_color $col)
echo -s -- $cwd
end
