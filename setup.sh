#! /bin/bash

# Copyright (C) Zalan Kemenczy
# Author: Zalan Kemenczy <zalan.k@gmail.com>

# This setup script populates user specific dotfiles and configures
# the development environment, including emacs.


TARGETLIST="\
.bashrc \
.profile \
.screenrc \
.emacs \
.emacs.conf \
local/share/elisp"

BYTE_COMPILE_ELISP_PACKAGES="\
cedet-bzr \
ecb"

Simlink to basic configuration files
for TARGET in $TARGETLIST ; do
    if [[ -e $HOME/$TARGET || -h $HOME/$TARGET ]] ; then
        echo "Warning, $HOME/$TARGET already exists, \
backing up as $TARGET.$(date +%Y%m%d-%H.%M.%S).setup.bak"
        mv $HOME/$TARGET $HOME/$TARGET.`date "+%Y%m%d-%H.%M.%S"`.setup.bak
    fi
    BASENAME=${TARGET##*/}
    NODOT=${BASENAME#.}
    ln -sf $(pwd)/setupfiles/$NODOT $HOME/$TARGET
done

# Initialize setup submodiles. These include:
#   -- ECB

git init submodule
git submodule update

# Set version of Emacs to byte-compile lisp packages, and perform OS
# specific configuration code

OS=$(uname -s)

test-for-emacs () {
    if [[ ! -x $EMACS ]] ; then
        echo "Warning: supported version of Emacs ($EMACS) not installed."        
        EMACS="NOT_INSTALLED/UNSUPPORTED"
    fi
    echo $EMACS
}    

case "$OS" in
    Darwin*)
        EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9"
        # Install and/or update homebrew and packages
        test-for-emacs
        if ! which brew; then
            ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
        fi
        brew update
        brew upgrade
        brew install node npm
        ;;
    Linux*)
        EMACS=$(which emacs)
        test-for-emacs
        ;;
    MINGW32_NT*|CYGWIN-NT*)
        # Test under proper cygwin env, using proper location of emacs-w32.exe
        EMACS="/usr/bin/emacs-w32.exe"
        test-for-emacs
        ;;
    *)
        test-for-emacs
        echo "Warning: unspported OS."
        ;;
esac

# Byte compile elisp packages
if [[ ! $EMACS == "NOT_INSTALLED/UNSUPPORTED" ]] ; then
    for PACKAGE in $BYTE_COMPILE_ELISP_PACKAGES ; do
        pushd setupfiles/elisp/$PACKAGE
        make EMACS=$EMACS
        popd
    done
fi

exit 0
