#!/bin/bash
# Usage: install-dotfile.sh <path to file to add, from list-dotfiles>
#          or
#        install-dotfile.sh -m <path to manifest file> -i <path to installation directory> <path to file to add, from list-dotfiles>


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
Usage: $0 <path to file to install, from list-dotfiles>
         or
       $0 -m <path to manifest file> -i <path to installation directory> <path to file to install, from list-dotfiles>

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

    INSTALL_FILE="$1";
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
    INSTALL_FILE="${@: -1}";
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


echo "You've requested to install the dotfiles $INSTALL_FILE."
echo "The full path to this repository is $DOTFILEHOME.";

pushd "$DOTFILEWAREHOUSE" 2>/dev/null 1>/dev/null;
#git pull origin master || { echo "Failed to fetch changes to dotfiles."; exit 1; } ;

for line in $(cat $DOTFILEMANIFEST); do
    FILE=$(echo "$line" | cut -d "|" -f 1);
    PATH_=$(echo "$line" | cut -d "|" -f 2);

    if [[ ! -e $DOTFILEWAREHOUSE/$FILE ]]; then
	echo "Corruption detected! Expected file $DOTFILEWAREHOUSE/$FILE does not exist.";
	exit 1;
    fi

    if [[ ! "$INSTALL_FILE" =~ "$FILE" ]] ; then 
	continue;
    fi

    echo "Installing $FILE with path $PATH_";

    if [[ -e "$PATH_" ]]; then
	EXISTING=$(readlink -f "$PATH_"  2>/dev/null) || \
	    { echo "Failed to read dotfile link."; exit 1;};
	if [[ "$EXISTING" =~ .*"$DOTFILEWAREHOUSE".* ]] ; then 
	    continue; 
	fi

	echo "Adding new dotfile with path $PATH_.";
	echo "  This dotfile already exists.";
	 
	if [[ -h "$PATH_" ]] ; then
	    echo "  The existing dotfile is a link.";
	    echo "  This will backup the source of the link with path $EXISTING.";
	fi
	
	if [[ -e "$EXISTING"".bak" ]] ; then
	    echo "  Backup already exists with path $EXISTING"".bak. Please fix.";
	    exit 1;
	fi

	mv "$EXISTING" "$EXISTING"".bak" || \
	    { echo "Failed to move existing dotfile to backup."; exit 1; };
	echo "  Backed up existing dotfile to $EXISTING"".bak.";
	echo "ln -s $DOTFILEWAREHOUSE/$FILE  $EXISTING";
	ln -s "$DOTFILEWAREHOUSE/$FILE" "$EXISTING" || \
            { echo "Failed to create link to new dotfile."; exit 1; };
	echo "  Successfully created dotfile with path $EXISTING.";
    else
	echo "New";

	echo "Adding new dotfile with path $PATH_.";
	ln -s "$DOTFILEWAREHOUSE/$FILE" "$PATH_" || \
            { echo "Failed to create link to new dotfile."; exit 1; } ;
	echo "  Successfully created dotfile with path $PATH_.";
    fi
done

popd 2>/dev/null 1>/dev/null;

echo "Complete.";
exit 0;