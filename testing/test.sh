#!/bin/zsh

if [[ ! -d ./warehouse ]]; then
    mkdir ./warehouse;
fi


git checkout testing || { exit 1; };
for file in TEST*.sh; do
    zsh $file;
done;
git checkout master;
