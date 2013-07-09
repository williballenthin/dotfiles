#!/bin/bash
# Usage: update-dotfiles.sh <path to file to add>
#          or
#        update-dotfiles.sh -i <path to installation directory>


#### CONFIGURE DEFAULT HERE:
if [[ ! "$DOTFILEHOME" ]]; then
  DOTFILEHOME="/home/willi/Git/dotfiles/";
fi
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

echo "You've requested to update the dotfiles repository."
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

git add -u || { echo "Failed to add changes to dotfiles."; exit 1; } ;
git commit -m "Updated changes to dotfiles.";
git push origin master;
popd 2>/dev/null 1>/dev/null;

echo "Complete.";
exit 0;
