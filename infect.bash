#!/bin/bash
if ! which stow; then
	echo "Install gnu stow"
	return 1
fi

stow -S -t "$HOME" *
