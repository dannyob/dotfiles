emulate sh  -c 'source /etc/profile'
declare -x EDITOR="emacs -nw"
declare -x SHORTHOST=`hostname`
declare -x PATH=$HOME/.local/bin:$PATH
declare -x GUIX_LOCPATH=$GUIX_LOCPATH:$HOME/.guix-profile/lib/locale
