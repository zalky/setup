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

# This is to make ECB byte-compile properly
REQUIRED_EMACS_VERSION="24.4.90.1"

MAC_EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9"
WIN_EMACS="/usr/bin/emacs-w32.exe"
LIN_EMACS=$(which emacs)


# Simlink to basic configuration files
for TARGET in $TARGETLIST ; do
    if [[ -e $HOME/$TARGET || -h $HOME/$TARGET ]] ; then
        echo "Warning, $HOME/$TARGET already exists, \
backing up as $TARGET.$(date +%Y%m%d-%H.%M.%S).setup.bak"
        mv $HOME/$TARGET $HOME/$TARGET.$(date "+%Y%m%d-%H.%M.%S").setup.bak
    fi
    BASENAME=${TARGET##*/}
    NODOT=${BASENAME#.}
    ln -sf $(pwd)/setupfiles/$NODOT $HOME/$TARGET
done

# Initialize setup submodiles. These include:
#   -- ECB
git init submodule
git submodule update

# Execute OS specific configuration steps, as well as set Emacs version.
OS=$(uname -s)

# This function returns 0 if version $1 is less than version $2,
# else it returns 1
version_less_than() {
    V="$1."
    V_REQ="$2."
    VER=${V%%.*}
    VER_REQ=${V_REQ%%.*}

    if (( $(echo "$VER < $VER_REQ" | bc -l) )) ; then
        return 0
    elif (( $(echo "$VER > $VER_REQ" | bc -l) )) ; then
        return 1
    else
        VER=${V#*.}
        VER_REQ=${V_REQ#*.}
        VER=${VER%.}
        VER_REQ=${VER_REQ%.}
        if (( $(echo "${V%%.*} == 0 && ${V_REQ%%.*} == 0" | bc -l) ))\
            && [[ -z $VER$VER_REQ ]] ; then
            return 1
        fi
        version_less_than ${VER:-"0"} ${VER_REQ:-"0"}
        return $?
    fi
}

# This function test to see if Emacs is installed and checks to see
# if version is greater than $REQUIRED_EMACS_VERSION at top of file
test-emacs () {
    # Which Emacs is installed?
    echo "Checking to see if supported version of Emacs is installed..."
    if [[ ! -x $EMACS ]] ; then
        echo "Warning: supported version of Emacs ($EMACS) not installed."        
        EMACS="NOT_INSTALLED/UNSUPPORTED"
    else
        # What version? Required version set at beginning of script.
        # Lots of string -> number conversions here.
        EMACS_VERSION=$($EMACS --version | egrep "GNU Emacs [[:digit:]]+" | \
            sed -E "s/GNU Emacs //")
        if version_less_than $EMACS_VERSION $REQUIRED_EMACS_VERSION ; then
            echo -e "Warning: version $EMACS_VERSION of Emacs breaks \
Emacs Code Browser! (Require >= $REQUIRED_EMACS_VERSION)"
            EMACS="NOT_INSTALLED/UNSUPPORTED"
        else
            echo -e "Using Emacs: $EMACS\nVersion: $EMACS_VERSION"
        fi
    fi
}    

# OS Specific configuration
# 1. Test for Emacs
# 2. Install important packages and tools
case "$OS" in
    Darwin*)
        EMACS=$MAC_EMACS
        test-emacs
        # Install and/or update homebrew and packages
        echo "Install and update homebrew..."
        if ! which brew; then
            ruby -e "$(curl -fsSL \
https://raw.githubusercontent.com/Homebrew/install/master/install)"
        fi
        brew update
        brew upgrade
        brew install node npm
        ;;
    Linux*)
        EMACS=$LIN_EMACS
        test-emacs
        ;;
    MINGW32_NT*|CYGWIN-NT*)
        # Test under proper cygwin env, using proper location of emacs-w32.exe
        EMACS=$WIN_EMACS
        test-emacs
        ;;
    *)
        test-emacs
        echo "Warning: unspported OS."
        ;;
esac

# Setup git to use Emacs editor with lite init file and byte compile
# all elisp packages
echo "Setting Emacs as default git editor..."
echo "Byte-compile using $EMACS"
if [[ ! $EMACS == "NOT_INSTALLED/UNSUPPORTED" ]] ; then
    git config --global core.editor "$EMACS --no-desktop -q --load \
~/.setup/setupfiles/emacs.conf/emacs-git.el"
    for PACKAGE in $BYTE_COMPILE_ELISP_PACKAGES ; do
        echo "Byte-compiling $PACKAGE..."
        pushd setupfiles/elisp/$PACKAGE &>/dev/null
        make EMACS=$EMACS &>/dev/null
        popd &>/dev/null
    done
fi

exit 0
