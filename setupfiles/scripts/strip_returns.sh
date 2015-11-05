#! /bin/bash

for FILE in $(find . -type f -exec grep -Il "" {} \; -and -print) ; do
    perl -pi -e 's/\r\n|\n|\r/\n/g'x $FILE
done

