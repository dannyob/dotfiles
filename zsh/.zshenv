eval `guix package -p /run/current-system/profile -p ~/.guix-profile --search-paths`
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "/run/setuid-programs/" ] ; then
    PATH="/run/setuid-programs:$PATH"
fi


if [ -x "`which vim`" ]; then
    declare -x EDITOR=vim
elif [ -x "`which nvim`" ]; then
    declare -x EDITOR=nvim
else
    declare -x EDITOR=vi
fi

VISUAL=$EDITOR
LESS="-RiMb256cX"
PAGER=less
MAILFOLDER=$HOME/Private/Mail
DEBFULLNAME="Danny O'Brien"
EMAIL=danny@spesh.com
DEBEMAIL=$EMAIL
SHORTHOST=`hostname -s`

if [ "$SHORTHOST" = "localhost" ]; then
    SHORTHOST=`hostname -a |  sed 's/ .*$//'`
fi
