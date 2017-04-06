#! /bin/bash

# Copyright (C) Zalan Kemenczy
# Author: Zalan Kemenczy <zalan.k@gmail.com>

# This setup script populates user specific dotfiles and configures
# the development environment, including emacs.

# TODO: This string could mangle for loop if filenames have spaces.
# Consider implementing as an array.
TARGETLIST="\
.bashrc \
.boot/boot.properties \
.boot/profile.boot \
.emacs \
.emacs.conf \
.gitconfig \
.gitignore_global \
.ipython/profile_default \
.lein/profiles.clj
.octaverc \
.profile \
.screenrc"

# Simlink basic configuration files in .setup/dotfiles to target locations
echo "Populating basic configuration files as symbolic links..."
for TARGET in $TARGETLIST ; do
    # If file already exists, even if it just a symlink, backup.
    if [[ -e $HOME/$TARGET ]] ; then
        echo "Warning, $HOME/$TARGET already exists, \
backing up as $TARGET.$(date +%Y%m%d-%H.%M.%S).setup.bak"
        mv $HOME/$TARGET $HOME/$TARGET.$(date "+%Y%m%d-%H.%M.%S").setup.bak
    fi
    BASE_DIR=$(dirname $TARGET)
    if [[ ! -d $HOME/$BASE_DIR ]] ; then
        mkdir -p $HOME/$BASE_DIR
    fi
    ln -sf $(pwd)/dotfiles/$TARGET $HOME/$TARGET    
done
