export PS1="\u@\h:\w$ "

export DATOMIC_HOME="$HOME/opt/datomic"

# Locally install binaries, not under version control
BIN="$HOME/local/bin"

# Scripts, these are under version control
SCRIPTS="$HOME/src/scripts"

export PATH="$DATOMIC_HOME/bin:$BIN:$SCRIPTS:/usr/local/sbin:$PATH"

# added by Anaconda2 4.3.1 installer
export PATH="$HOME/anaconda/bin:$PATH"
