#!/bin/bash
# Usage: list-dotfiles.sh
#          or
#        list-dotfiles.sh -i <path to installation directory>


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
Usage: $0
         or
       $0 -i <path to installation directory>

This script lists tracked configuration files ("dotfiles") in a tracked repository.

OPTIONS:
   -i      (optional) The path to a dotfiles installation directory
EOF
}


## MAIN
DOTFILEMANIFEST="$DOTFILEHOME/manifest.txt";
if   [[ $# -ne 0 ]] ; then
    if [[ $(expr $# % 2) -ne 0 ]]; then
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

echo "You've requested to list the dotfiles repository."
echo "The full path to this repository is $DOTFILEHOME.";

pushd "$DOTFILEWAREHOUSE" 2>/dev/null 1>/dev/null;

for line in $(cat $DOTFILEMANIFEST); do
    FILE=$(echo "$line" | cut -d "|" -f 1);
    PATH_=$(echo "$line" | cut -d "|" -f 2);

    echo "\"$FILE\" (key)"
    echo "    found at: $PATH_"
    
    if [[ ! -e $DOTFILEWAREHOUSE/$FILE ]]; then
	echo "Corruption detected! Expected file $DOTFILEWAREHOUSE/$FILE does not exist.";
	exit 1;
    fi

    if [[ -e "$PATH_" ]]; then
	EXISTING=$(readlink -f "$PATH_"  2>/dev/null) || \
	    { echo "Failed to read dotfile link."; exit 1;};
	IS_EXISTANT=1;
	if [[ "$EXISTING" =~ .*"$DOTFILEWAREHOUSE".* ]] ; then 
	    IS_ALREADY_TRACKED=1;
	elif [[ -h "$PATH_" ]] ; then
	    IS_OTHER_LINK=1;
	fi
	 
	if [[ -e "$EXISTING"".bak" ]] ; then
	    HAS_BACKUP=1
	fi
    else
	IS_EXISTANT=0;
	IS_ALREADY_TRACKED=0;
	IS_OTHER_LINK=0;
	HAS_BACKUP=0;
    fi

    if [[ $IS_EXISTANT -eq 1 ]]; then
	echo "    already exists: True";
    elif [[ $IS_EXISTANT -eq 0 ]]; then
	echo "    already exists: False";
    fi

    if [[ $IS_ALREADY_TRACKED -eq 1 ]]; then
	echo "    already tracked: True";
    elif [[ $IS_ALREADY_TRACKED -eq 0 ]]; then
	echo "    already tracked: False";
    fi

    if [[ $IS_OTHER_LINK -eq 1 ]]; then
	echo "    other link: True";
    elif [[ $IS_OTHER_LINK -eq 0 ]]; then
	echo "    other link: False";
    fi

    if [[ $HAS_BACKUP -eq 1 ]]; then
	echo "    backup exists: True";
    elif [[ $HAS_BACKUP -eq 0 ]]; then
	echo "    backup exists: False";
    fi
done

popd 2>/dev/null 1>/dev/null;

echo "Complete.";
exit 0;