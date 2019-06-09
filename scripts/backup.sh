#! /bin/bash

system_backup_dirs="\
~/.emacs.d \
~/.gitconfig \
~/Desktop \
~/Documents \
~/Downloads \
~/Library \
~/Movies \
~/Music \
~/Pictures \
~/local \
~/opt \
~/src"

if [[ $# -gt 0 ]]; then
    target=$1
else
    echo "Usage: Must specify a target directory"
    exit 1
fi

eval rsync -avvz --partial --progress --delete $system_backup_dirs $target
