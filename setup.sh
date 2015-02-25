#! /bin/bash

# Author: Zalan Kemenczy
# Emacs v: 24.4.90

# Run setup script to populate dotfiles and configure dev environment,
# including emacs.


TARGETLIST="\
.bashrc \
.profile \
.screenrc \
.emacs \
.emacs.conf \
local/share/elisp"

# Simlink to basic configuration files
for TARGET in $TARGETLIST ; do
    if [[ -e $HOME/$TARGET ]] || [[ -h $HOME/$TARGET ]] ; then
        echo "Warning, $HOME/$TARGET already exists, \
backing up as $TARGET.`date +%Y%m%d-%H.%M.%S`.setup.bak"
        mv $HOME/$TARGET $HOME/$TARGET.`date "+%Y%m%d-%H.%M.%S"`.setup.bak
    fi
    BASENAME=${TARGET##*/}
    NODOT=${BASENAME#.}
    ln -sf `pwd`/setupfiles/$NODOT $HOME/$TARGET
done

exit 0
