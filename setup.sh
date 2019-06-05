#! /bin/bash

# Copyright (C) Zalan Kemenczy
# Author: Zalan Kemenczy <zalan.k@gmail.com>

# This setup script symlinks user specific development environment
# files.

# However, gitconfig dotfile contians user and email that we do not
# want symlinked to source. People might accidentally start making
# commits under the wrong identity. Therefore, get git username and
# email from command line options and generate gitconfig rather than
# symlink. This means gitconfig is not symlinked.

while getopts ":u:e:" opt ; do
    case $opt in
        u)
            USERNAME=$OPTARG
            ;;
        e)
            EMAIL=$OPTARG
            ;;
        \?)
            echo "Invalid Option: -$OPTARG" 1>&2
            exit 1
            ;;
        :)
            echo "Invalid Option: -$OPTARG requires an argument" 1>&2
            exit 1
            ;;
    esac
done

if [[ ! $USERNAME ]] ; then
    echo "use -u to supply a username"
    exit 1
fi

if [[ ! $EMAIL ]] ; then
    echo "use -e to supply an email"
    exit 1
fi

# gitconfig stored in safe format in source. All user specific
# parameters are referenced by $USERNAME and $EMAIL. See file for
# details.
source dotfiles/.gitconfig

if [[ -e $HOME/.gitconfig ]] ; then
    echo "Warning, $HOME/.gitconfig already exists, \
backing up as $TARGET.$(date +%Y%m%d-%H.%M.%S).setup.bak"
    mv $HOME/.gitconfig $HOME/.gitconfig.$(date "+%Y%m%d-%H.%M.%S").setup.bak
fi

echo "$GITCONFIG" > $HOME/.gitconfig

# All other dotfiles can be handled together.

# TODO: This string could mangle for loop if filenames have spaces.
# Consider implementing as an array.

DOTFILES="\
.bashrc \
.boot/boot.properties \
.boot/profile.boot \
.emacs \
.emacs.conf \
.gitignore_global \
.ipython/profile_default \
.lein/profiles.clj
.octaverc \
.tmux.conf \
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
