function nb
    if set -l dir (git rev-parse --show-toplevel 2>/dev/null); and test -e $dir/CMakeLists.txt
        if ! test -d $dir/build
            mkdir $dir/build
            cmake -G Ninja $dir $dir/build
        end
        ninja -C $dir/build $argv
    end
end
