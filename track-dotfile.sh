#!/bin/bash

#### CONFIGURE HERE:
DOTFILEHOME="/home/willi/projects/dotfiles/";
#### END CONFIGURE


DOTFILEWAREHOUSE="$DOTFILEHOME/warehouse/";
DOTFILEMANIFEST="$DOTFILEHOME/manifest.txt";

FILE="$1";
FULLPATH=$(readlink -f $FILE);
BASENAME=$(basename $FILE);
NEWPATH="$DOTFILEWAREHOUSE/$BASENAME"

echo "You've requested to track the dotfile $FILE.";
echo "The full path to this file is $FULLPATH.";

if [[ "$FULLPATH" == *"$DOTFILEWAREHOUSE"* ]] ; then
    # already tracked
    echo "This file is already tracked by dotfiles.";
    exit 0;
fi


if [ -h "$FILE" ] ; then
    # is link

    # TODO(wb) copy target of link and redirect link
    echo "The path provided is a symlink.";
    echo "symlink support not implemented.";
else
    # normal file

    mv "$FULLPATH" "$NEWPATH";
    ln -s "$NEWPATH" "$FULLPATH";
    echo "$BASENAME|$FULLPATH" >> "$DOTFILEMANIFEST";
    echo "Updated links to the dotfile.";

    pushd "$DOTFILEWAREHOUSE" 2>/dev/null 1>/dev/null;
    git add "$BASENAME";
    git commit -m "track-dotfile.sh added dotfile $BASENAME";
    popd 2>/dev/null 1>/dev/null;
fi

echo "Complete.";





