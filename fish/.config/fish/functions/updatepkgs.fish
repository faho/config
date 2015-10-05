function updatepkgs
	cd ~/dev/build/current
	git submodule foreach git pull -q origin master
	makepkgs *
end
