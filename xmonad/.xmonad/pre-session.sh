xsetroot -cursor_name left_ptr  # __XCURSOR__
xrdb -merge ~/.Xresources  # __XRDB__
xmodmap ~/.Xmodmap  # __XMODMAP__
compton -b
#bash ~/.wallpaper/wallpaper.sh &  # __WALLPAPER__
xautolock -time 5 -locker 'i3lock' &
#(bash ~/.xmonad/bar.sh | dzen2 -geometry -600+0 -bg black -fn 'Droid Sans Mono-7') &
trayer --edge top --align left --widthtype request --height 15 &
dunst -config ~/.dunstrc &
notify-send "welcome.";
