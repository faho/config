function makepkgs --description "Build multiple packages with makepkg"
    if [ (count $argv) -eq 0 ]
        echo "Please specify packages"
        return
    end
    set -l built 0 # has a new package been built last run? (0=true, 1=false)
    while [ $built -eq 0 -a (count $argv) -gt 0 ]
        set -l built 1
        for pkg in $argv
            echo "Making $pkg"
            pushd $pkg
            # Install dependencies and the resulting package, but only when needed
            # Don't remove dependencies because they might be needed by the next package
            makepkg -si --needed
            if [ $status -eq 0 ]
                # Remove package that has been built successfully
                set -e argv[(contains -i $pkg $argv)]
                set built 0
            end
            popd
        end
    end
    if [ $built -gt 0 ]
        echo "No new packages could be built, please check"
    end
end
