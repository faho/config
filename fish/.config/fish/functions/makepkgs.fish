function makepkgs --description 'Build multiple packages with makepkg'
	if [ (count $argv) -eq 0 ]
        echo "Please specify packages"
        return
    end

	set -l opts
	set -l packages
	for arg in $argv
		switch $arg
			case "-*"
				set opts $opts $arg
			case "*"
				set packages $packages $arg
		end
	end
	# directories could be relative, so always return to the original directory
	set -l startdir $PWD
    set -l failed # packages that failed to build
    while [ (count $packages) -gt 0 ]
        for pkg in $packages
			cd $startdir
            echo "Making $pkg"
            cd $pkg # directories could also be absolute, so this needs to be a separate cd
			# Check .SRCINFO for dependencies
			if set -l aurdeps (test -r .SRCINFO; or makepkg --printsrcinfo > .SRCINFO; faho_getaurdeps < .SRCINFO)
				if not set -l missing (pacman -T -- $aurdeps)
					if not set -q packages[2]
						# Abort instead of skipping if we are the last pkg - or we'd loop endlessly
						set failed $failed $pkg
						set -e packages
						break
					else
						# Skip because there's unresolvable deps (likely from the AUR)
						# These might still be in the transaction
						continue
					end
				end
			end
            # Install dependencies (-s) and the resulting package (-i), but only when needed (--needed)
            # Don't remove dependencies because they might be needed by the next package (that would be -r)
            # Also log output (-L) and clean on successful build (-c)
            makepkg -sicL --needed; or set failed $failed $pkg
            set -e packages[(contains -i $pkg $packages)]
        end
    end
    if [ (count $failed) -gt 0 ]
        echo (count $failed) packages failed to build: $failed
    end
	cd $startdir
end
