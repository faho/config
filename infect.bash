#!/bin/bash
if ! which stow > /dev/null 2>&1 ; then
    echo "Install gnu stow"
    exit 1
fi

for d in *; do
    [[ ! -d $d ]] && continue
    stow -S -t "$HOME" "$d"
done
