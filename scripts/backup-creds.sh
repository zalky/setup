#! /bin/bash

credential_backup_dirs="\
~/.2xauth \
~/.aws \
~/.passwords \
~/.ssh \
~/.gnupg"

if [[ $# -gt 0 ]]; then
    target=$1
else
    echo "Usage: Must specify a target directory"
    exit 1
fi

eval tar -cvzf - $credential_backup_dirs | gpg --cipher-algo AES256 -z 0 --output $target/cred.tar.gz.gpg --symmetric
