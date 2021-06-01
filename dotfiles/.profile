export PS1="\u@\h:\w$ "

export DATOMIC_HOME="$HOME/opt/datomic"
export ES_HOME="$HOME/opt/elasticsearch"
export STARDOG_BIN="$HOME/opt/stardog/bin"
export STARDOG_HOME="$HOME/opt/stardog-data"

# Locally install binaries, not under version control
BIN="$HOME/local/bin"

# Scripts, these are under version control
SCRIPTS="$HOME/src/scripts"

export PATH="$STARDOG_BIN:$DATOMIC_HOME/bin:$ES_HOME/bin:$BIN:$SCRIPTS:/usr/local/sbin:$PATH"

# added by Anaconda2 4.3.1 installer
export PATH="$HOME/anaconda/bin:$PATH"

# added by Anaconda3 5.1.0 installer
export PATH="/Users/zalan/anaconda3/bin:$PATH"

# Aliases

alias rsync-backup="rsync -avvz --partial --progress --delete"
# added by Anaconda3 2019.03 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$(CONDA_REPORT_ERRORS=false '/anaconda3/bin/conda' shell.bash hook 2> /dev/null)"
if [ $? -eq 0 ]; then
    \eval "$__conda_setup"
else
    if [ -f "/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/anaconda3/etc/profile.d/conda.sh"
        CONDA_CHANGEPS1=false conda activate base
    else
        \export PATH="/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda init <<<
