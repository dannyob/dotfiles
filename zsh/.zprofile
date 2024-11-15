emulate sh -c 'source /etc/profile'
emulate sh -c 'source ~/.profile'
declare -x SHORTHOST=`hostname`
declare -x PATH=$HOME/go/bin:$HOME/.cargo/bin:$HOME/.local/bin:$HOME/.deno/bin:$HOME/.config/emacs/bin:$PATH
declare -x GUIX_LOCPATH=$GUIX_LOCPATH:$HOME/.guix-profile/lib/locale
declare -x MAILDIR=$HOME/Private/mail
declare -x XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share/:/usr/share/
declare -x XDG_SCREENSHOTS_DIR=$HOME/tmp
declare -x PYTHONHISTFILE=$HOME/.cache/python_history
