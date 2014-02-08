xsetroot -cursor_name left_ptr  # __XCURSOR__
xrdb -merge ~/.Xresources  # __XRDB__
xmodmap ~/.Xmodmap  # __XMODMAP__
compton -b  # __COMPTON__
bash ~/.wallpaper/wallpaper.sh &  # __WALLPAPER__
xscreensaver &
$(bash bar.sh | dzen2 -ta r -fn 'Droid Sans Mono-7') &
~/.cabal/bin/xmonad  # __XMONAD__
