#!/bin/bash
# Usage: update-dotfiles.sh <path to file to add>
#          or
#        update-dotfiles.sh -i <path to installation directory>


#### CONFIGURE DEFAULT HERE:
DOTFILEHOME="/home/willi/projects/dotfiles/";
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
Usage: $0
         or
       $0 -i <path to installation directory>

This script updates any changes to configuration files ("dotfiles") to a tracked repository.

OPTIONS:
   -i      (optional) The path to a dotfiles installation directory
EOF
}


## MAIN
DOTFILEMANIFEST="$DOTFILEHOME/manifest.txt";
if   [[ $# -ne 0 ]] ; then
    if [[ $(expr $# % 2) -ne 1 ]]; then
	echo "Error: incorrect number of arguments provided.";
	usage;
	exit 1;
    fi

    while getopts “hi:” OPTION
    do
	case $OPTION in
            h)
		usage
		exit 0
		;;
	    i)
		DOTFILEHOME=$OPTARG;
		DOTFILEMANIFEST="$DOTFILEHOME/manifest.txt";
		;;
            ?)
		usage
		exit
		;;
	esac
    done
fi

DOTFILEMANIFEST=$(readlink -f "$DOTFILEMANIFEST");
DOTFILEWAREHOUSE=$(readlink -f "$DOTFILEHOME/warehouse/");

if [[ ! -e "$DOTFILEWAREHOUSE" ]]; then 
    echo "Warehouse directory does not exist at: $DOTFILEWAREHOUSE.";
    exit 1;
fi

if [[ ! -e "$DOTFILEMANIFEST" ]]; then 
    echo "Manifest file does not exist at: $DOTFILEMANIFEST.";
    exit 1;
fi

echo "You've requested to update the dotfiles repository."
echo "The full path to this repository is $DOTFILEHOME.";

pushd "$DOTFILEWAREHOUSE" 2>/dev/null 1>/dev/null;
git pull origin master || { echo "Failed to fetch changes to dotfiles."; exit 1; } ;
git add -u || { echo "Failed to add changes to dotfiles."; exit 1; } ;
git commit -m "Updated changes to dotfiles." || { echo "Failed to commit updates to dotfiles in repository."; exit 1; } ;
git push origin master;
popd 2>/dev/null 1>/dev/null;
fi

echo "Complete.";
exit 0;