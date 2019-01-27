export GUIX_PROFILE=$HOME/.guix-profile
export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale
. $GUIX_PROFILE/etc/profile

xsetroot -bg black
xsetroot -cursor_name left_ptr

# Applets and agents
xsettingsd &
nm-applet &
xbindkeys_autostart

xrdb -merge .Xresources
setxkbmap -option caps:hyper
xmodmap -e 'clear Mod4'
xmodmap -e 'clear Mod3'
xmodmap -e 'keycode 66 = Escape'
xmodmap -e 'add mod3 = Escape Hyper_L'
xmodmap -e 'add Mod4 = Super_L Super_R'


dbus-launch --exit-with-session /home/danny/.local/bin/dwm
