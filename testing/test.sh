#!/bin/zsh

if [[ ! -d ./warehouse ]]; then
    mkdir ./warehouse;
fi


git checkout testing || { exit 1; };
git merge master;
for file in TEST*.sh; do
    zsh $file;
done;
git add -u;
git commit -m "testing run completing.";
git checkout master;

