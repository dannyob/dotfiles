#emulate sh  -c 'source /etc/profile'
declare -x EDITOR="emacsclient -s /var/run/user/1000/emacs/server -a nvim -nw"
declare -x ALTERNATIVE_EDIT="nvim"
declare -x SHORTHOST=`hostname`
declare -x PATH=$HOME/.local/bin:$PATH
declare -x GUIX_LOCPATH=$GUIX_LOCPATH:$HOME/.guix-profile/lib/locale
