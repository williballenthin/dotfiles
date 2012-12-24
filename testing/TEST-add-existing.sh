#!/bin/zsh

OUT="/tmp/out";
TESTFILE="file.txt";
echo "123" >> "$TESTFILE";
THIS="$0";

cleanup() {
    rm $OUT;
    rm $TESTFILE;
    echo -ne "" > ./manifest.txt;
    git rm -rf "./warehouse" 2>/dev/null 1>/dev/null;
    git commit -m "TESTING cleanup." 2>/dev/null 1>/dev/null;
    mkdir "./warehouse" 2>/dev/null 1>/dev/null;
    git add "./warehouse" 2>/dev/null 1>/dev/null;
    git commit -m "TESTING cleanup" 2>/dev/null 1>/dev/null;
}

fail() {
    ERRORCODE=$1;
    echo "FAILED $THIS ($ERRORCODE)";
    cat $OUT | sed -e "s/^\(.\)/  \1/g";
    cleanup;
    exit;
}



../track-dotfile.sh -i . "$TESTFILE" 2>$OUT 1>$OUT;



# ensure script ran
if [[ $? -ne 0 ]]; then
    fail 0;
fi

# check file exists
if [[ ! -e file.txt ]]; then
    fail 1;
fi

# check file is now a link
if [[ ! -h file.txt ]]; then
    fail 2;
fi

NEWPATH=$(cat manifest.txt | grep "$TESTFILE" | cut -d "|" -f 1);
OLDPATH=$(cat manifest.txt | grep "$TESTFILE" | cut -d "|" -f 2);

# check key exists in manifest
if [[ "$NEWPATH" == "" ]]; then 
    fail 3;
fi

# check new file exists
if [[ ! -f "./warehouse/$NEWPATH" ]]; then
    fail 4;
fi

# check manifest link to original file
if [[ "$OLDPATH" != "$(pwd)/$TESTFILE" ]]; then 
    fail 5;
fi



../track-dotfile.sh -i . "$TESTFILE" 2>$OUT 1>$OUT;



# ensure script failed
if [[ $? -eq 0 ]]; then
    fail 6;
fi


echo "passed $0";
cleanup
