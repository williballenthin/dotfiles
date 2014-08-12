xsetroot -cursor_name left_ptr  # __XCURSOR__
xrdb -merge ~/.Xresources  # __XRDB__
xmodmap ~/.Xmodmap  # __XMODMAP__
compton -b  # __COMPTON__
bash ~/.wallpaper/wallpaper.sh &  # __WALLPAPER__
xautolock -time 5 -locker 'i3lock -g' &
(bash ~/.xmonad/bar.sh | dzen2 -geometry -600+0 -bg black -fn 'Droid Sans Mono-7') &
trayer --edge top --align left --widthtype request --height 15 &
killall xfce4-notifyd  # sorry, but there's no associated "autostart" file per-user
dunst -config ~/.dunstrc &
~/.cabal/bin/xmonad  # __XMONAD__
