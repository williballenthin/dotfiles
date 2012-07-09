#!/bin/zsh

if [[ ! -d ./warehouse ]]; then
    mkdir ./warehouse;
fi

for file in TEST*.sh; do
    zsh $file;
done;
