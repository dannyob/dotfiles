. /etc/profile
. $HOME/.profile

export GUIX_PROFILE=$HOME/.guix-profile
export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale
. $GUIX_PROFILE/etc/profile

setxkbmap -option caps:hyper
xmodmap -verbose $HOME/.config/xmodmaprc
xrdb -merge .Xresources

xsetroot -bg black
xsetroot -cursor_name left_ptr

# Applets and agents
xsettingsd &
nm-applet &
gbar &
xbindkeys_autostart

dbus-launch --exit-with-session /home/danny/.local/bin/dwm
