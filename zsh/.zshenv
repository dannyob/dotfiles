if [ -d "$HOME/.guix-profile" ] ; then
    eval `guix package -p /run/current-system/profile -p ~/.guix-profile --search-paths`
fi

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

declare -x VISUAL=$EDITOR
declare -x LESS="-RiMb256cX"
declare -x PAGER=less
declare -x MAILFOLDER=$HOME/Private/Mail
declare -x DEBFULLNAME="Danny O'Brien"
declare -x EMAIL=danny@spesh.com
declare -x DEBEMAIL=$EMAIL
declare -x SHORTHOST=`hostname -s`

if [ "$SHORTHOST" = "localhost" ]; then
    SHORTHOST=`hostname -a |  sed 's/ .*$//'`
fi
