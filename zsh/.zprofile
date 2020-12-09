emulate sh -c 'source /etc/profile'
#GUIX_PROFILE=$HOME/.guix-profile emulate sh  -c 'source $HOME/.guix-profile/etc/profile'
#GUIX_PROFILE=$HOME/.config/guix/current emulate sh  -c 'source $HOME/.config/guix/current/etc/profile'
declare -x SHORTHOST=`hostname`
declare -x PATH=$HOME/.local/bin:$PATH
declare -x GUIX_LOCPATH=$GUIX_LOCPATH:$HOME/.guix-profile/lib/locale
declare -x MAILDIR=$HOME/Private/mail
declare -x XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share/:/usr/share/
