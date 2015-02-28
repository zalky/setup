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

ELISP_PACKAGES="\
cedet-bzr \
ecb"

# Simlink to basic configuration files
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

# Initialize git submodiles. These include:
#   -- ECB

git init submodule
git submodule update

# OS specific setup
if [[ $(uname) == "Darwin" ]]; then
    EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9"
    # Install and/or update homebrew and packages
    if ! which brew; then
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    brew update
    brew upgrade
elif [[ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]]; then
    # Test under proper cygwin env, using proper location of emacs-w32.exe
    EMACS="/usr/bin/emacs-w32.exe"
elif [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
    EMACS=$(which emacs)
else
    EMACS="Not installed!"
fi

# Byte compile elisp packages
if [[ $EMACS == "Not installed" ]] ; then
    echo "Warning, proper version of Emacs not installed ($(which emacs))."
else
    for PACKAGE in $ELISP_PACKAGES ; do
        pushd setupfiles/elisp/$PACKAGE
        make EMACS=$EMACS
        popd
    done
fi

exit 0
