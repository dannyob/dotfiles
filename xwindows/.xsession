. /etc/profile
. $HOME/.profile

export GUIX_PROFILE=$HOME/.guix-profile
export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale
. $GUIX_PROFILE/etc/profile

# Applets and agents
xsettingsd &
nm-applet &
xbindkeys_autostart
synology-drive &

# Fiddle with keyboard after xkb has done its worst
(sleep 3; 
 setxkbmap -option caps:hyper
 setxkbmap -option compose:prsc
 xmodmap -verbose $HOME/.config/xmodmaprc
 guile `which gbar` &
 x-terminal-emulator &
 ) &
xrdb -merge .Xresources

xsetroot -bg black
xsetroot -cursor_name left_ptr

/home/danny/.local/bin/dwm
