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

echo "abc" >> "$TESTFILE";

../update-dotfiles.sh -i . 2>$OUT 1>$OUT;



# ensure script ran
if [[ $? -ne 0 ]]; then
    fail 0;
fi

echo "passed $0";
cleanup
