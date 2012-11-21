#!/bin/bash
# Usage: track-dotfile.sh <path to file to add>
#          or
#        track-dotfile.sh -m <path to manifest file> -i <path to installation directory> <path to file to add>


#### CONFIGURE DEFAULT HERE:
DOTFILEHOME="/home/willi/Git/dotfiles/";
#### END CONFIGURE DEFAULT


## VARIABLES
FULLPATH=
BASENAME=
KEY=
NEWPATH=


## FUNCTIONS
usage()
{
cat << EOF
Usage: $0 <path to file to add>
         or
       $0 -m <path to manifest file> -i <path to installation directory> <path to file to add>

This script adds configuration files ("dotfiles") to a tracked repository.

OPTIONS:
   -m      (optional) The path to a manifest file
   -i      (optional) The path to a dotfiles installation directory
EOF
}


## MAIN
if   [[ $# -eq 0 ]] ; then
    echo "Error: no arguments provided.";
    usage;
    exit 1;
elif [[ $# -eq 1 ]]; then
    DOTFILEMANIFEST="$DOTFILEHOME/manifest.txt";

    FILE="$1";
else
    DOTFILEMANIFEST="$DOTFILEHOME/manifest.txt";
    _SETMANIFEST=0

    if [[ $(expr $# % 2) -ne 1 ]]; then
	echo "Error: incorrect number of arguments provided.";
	usage;
	exit 1;
    fi

    while getopts “hm:i:” OPTION
    do
	case $OPTION in
            h)
		usage
		exit 0
		;;
	    m)
		DOTFILEMANIFEST="$OPTARG";
		_SETMANIFEST=1;
		;;
	    i)
		DOTFILEHOME=$OPTARG;

		if [[ $_SETMANIFEST -ne 1 ]]; then 
		    DOTFILEMANIFEST="$DOTFILEHOME/manifest.txt";
		fi
		;;
            ?)
		usage
		exit
		;;
	esac
    done

    # see: http://stackoverflow.com/questions/1853946/getting-the-last-argument-passed-to-a-shell-script
    FILE="${@: -1}";
fi

DOTFILEMANIFEST=$(readlink -f "$DOTFILEMANIFEST");
DOTFILEWAREHOUSE=$(readlink -f "$DOTFILEHOME/warehouse/");
FULLPATH=$(readlink -f $FILE 2>/dev/null) || { echo "Failed to read dotfile link."; exit 1;};
BASENAME=$(basename $FILE 2>/dev/null) || { echo "Failed to determine basename of dotfile."; exit 1;};
# we use this key rather than the basename
# because the filename may not be unique.
# for example, /home/willi/.config/terminator/config
# yields a basename of "config"
KEY="$BASENAME-$RANDOM";
NEWPATH=$(readlink -f "$DOTFILEWAREHOUSE/$KEY");

if [[ ! -e "$DOTFILEWAREHOUSE" ]]; then 
    echo "Warehouse directory does not exist at: $DOTFILEWAREHOUSE.";
    exit 1;
fi

if [[ ! -e "$DOTFILEMANIFEST" ]]; then 
    echo "Manifest file does not exist at: $DOTFILEMANIFEST.";
    exit 1;
fi

if [[ ! -e "$FILE" ]]; then 
    echo "Dotfile does not exist at: $FILE.";
    exit 1;
fi

echo "You've requested to track the dotfile $FILE.";
echo "The full path to this file is $FULLPATH.";

if [[ "$FULLPATH" == *"$DOTFILEWAREHOUSE"* ]] ; then
    # already tracked
    echo "This file is already tracked by dotfiles.";
    exit 1;
fi

if [ -h "$FILE" ] ; then
    # is link

    # TODO(wb) copy target of link and redirect link
    echo "The path provided is a symlink.";
    echo "symlink support not implemented.";
    exit 1;
else
    # normal file

    mv "$FULLPATH" "$NEWPATH"    || { echo "Failed to copy dotfile to repository. Please review state."; exit 1; } ;
    ln -s "$NEWPATH" "$FULLPATH" || { echo "Failed to create link to dotfile. Please review state."; exit 1; } ;
    echo "$KEY|$FULLPATH" >> "$DOTFILEMANIFEST" || { echo "Failed to update manifest. Please review state."; exit 1; } ;
    echo "Updated links to the dotfile.";

    pushd "$DOTFILEWAREHOUSE" 2>/dev/null 1>/dev/null;
    git add "$NEWPATH" || { echo "Failed to commit dotfile in repository."; exit 1; } ;
    git add "$DOTFILEMANIFEST" || { echo "Failed to commit manifest in repository."; exit 1; } ;
    git commit -m "track-dotfile.sh added dotfile $BASENAME" || { echo "Failed to commit dotfile in repository."; exit 1; } ;
    popd 2>/dev/null 1>/dev/null;
fi

echo "Complete.";
exit 0;