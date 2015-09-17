function mkfish
	if count $argv > /dev/null
		cd ~/dev/build/fish-shell-git
		sed -e "s|source=.*|source=('git+file:///home/alfa/dev/fish-shell#branch=$argv')|" -i PKGBUILD
		makepkg -sir
	else
		cd ~/dev/fish-shell/
		git pull origin
		cd ~/dev/build/fish-shell-git
		sed -e "s|source=.*|source=('git+file:///home/alfa/dev/fish-shell#branch=master')|" -i PKGBUILD
		makepkg -sir
	end
end
