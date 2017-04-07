#! /bin/bash

# Copyright (C) Zalan Kemenczy
# Author: Zalan Kemenczy <zalan.k@gmail.com>

# This setup script symlinks user specific development environment
# files.

# TODO: This string could mangle for loop if filenames have spaces.
# Consider implementing as an array.

DOTFILES="\
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

echo "Symlinking basic configuration files..."
for TARGET in $DOTFILES ; do
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

echo "Symlinking elisp folder..."
if [[ -e $HOME/local/share/elisp ]] ; then
    echo "Warning, $HOME/local/share/elisp already exists, \
backing up as $HOME/local/share/elisp.$(date +%Y%m%d-%H.%M.%S).setup.bak"
    mv $HOME/local/share/elisp $HOME/local/share/elisp.$(date "+%Y%m%d-%H.%M.%S").setup.bak
fi
if [[ ! -d $HOME/local/share ]] ; then
    mkdir -p $HOME/local/share
fi
ln -sf $(pwd)/elisp $HOME/local/share/elisp
