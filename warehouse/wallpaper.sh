#!/bin/sh

while true; do
    F=$(find ~/.wallpaper -type f \( -name '*.jpg' -o -name '*.png' -o -name '*.gif' \) | shuf -n1)
    if [ $(du -b "$F" | cut -f 1) -ge 100000 ]; then
        feh --bg-max "$F";
    else
        feh --bg-tile "$F";
    fi
    sleep 120m
done
