#!/bin/bash

OUT="/tmp/out";

../track-dotfile.sh -i /this-dir-dne existing-file.txt 2>$OUT 1>$OUT;
RESULT="$?";

if [[ $RESULT -ne 0 ]]; then
  echo "passed $0.";
else
  echo "FAILED $0.";
  cat $OUT | sed -e "s/^\(.\)/  \1/g";
fi

rm $OUT;

