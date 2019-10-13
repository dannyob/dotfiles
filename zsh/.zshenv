if [ -d "$HOME/.guix-profile" ] ; then
    OLDPATH=$PATH
    eval `guix package -p /run/current-system/profile -p ~/.guix-profile --search-paths`
    PATH=$PATH:$OLDPATH
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

if [ "$SHORTHOST" = "localhost" ]; then
    SHORTHOST=`hostname -a |  sed 's/ .*$//'`
fi

declare -x VISUAL=$EDITOR
declare -x LESS="-RiMb256cX"
declare -x PAGER=less
declare -x SHORTHOST=`hostname -s`
declare -x MAILFOLDER=$HOME/Private/Mail
declare -x DEBFULLNAME="Danny O'Brien"
declare -x EMAIL=danny@spesh.com
if [ "$SHORTHOST" = "lifeboat" ]; then
    declare -x EMAIL=danny@eff.org
fi
declare -x DEBEMAIL=$EMAIL


declare -x GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
. "$HOME/.guix-profile/etc/profile"
