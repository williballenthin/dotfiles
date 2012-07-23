#!/bin/zsh

OUT="/tmp/out";
TESTFILE="file.txt";
echo "123" >> "$TESTFILE";
THIS="$0";

cleanup() {
    rm $OUT;
    rm $TESTFILE;
    rm -rf "./123" 2>/dev/null;
    rm -rf "./123.bak" 2>/dev/null;
    echo -ne "" > "./manifest.txt";
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



HERE=$(readlink -f .);
echo "123" > "./warehouse/123";
echo "890" > "./123";
cp "./123" "./123.bak";
echo "123|$HERE/123" > "./manifest.txt";


../update-dotfiles.sh -i . 2>$OUT 1>$OUT;



# ensure script ran
if [[ $? -eq 0 ]]; then
    fail 0;
fi

# check file exists
if [[ ! -e "./123" ]]; then
    fail 1;
fi

# check backup file exists
if [[ ! -e "./123.bak" ]]; then
    fail 2;
fi

# check file is now a link
if [[ -h "./123" ]]; then
    fail 3;
fi

# check backup file is now a link
if [[ -h "./123.bak" ]]; then
    fail 4;
fi


echo "passed $0";
cleanup