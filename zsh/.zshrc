# last chance
declare -x SHORTHOST=${SHORTHOST:=$(hostname -s)}

# I like vim-syle bindings
bindkey -A viins main

# My prompt, which if pasted, is null action under zsh so you can copy and paste
declare -x PS1=': %m %1(j.[%j].) %~%#; '

if [[ -n $GUIX_ENVIRONMENT ]]
then
declare -x PS1=': [] %m %1(j.[%j].) %~%#; '
fi

# More generous umask -- people in my user group can also write my files
umask 002

# allow directories to be typed without 'cd'
setopt auto_cd
# always push directories
setopt auto_pushd
# we can cd to stuff added with 'hash -d'
setopt cdable_vars
# stops the prompt overwriting non-cr'd results
unsetopt promptcr 
# echo back history substitutions instead of just doing them
setopt hist_verify
# Multiple zsh shells share their history
setopt share_history
# Compact listings of shell history
setopt list_packed
# Ignore repeated commands
setopt hist_ignore_dups
# Remove superfluous blank lines
setopt hist_reduce_blanks

##
# ALIASES
##

# colored ls with hyperlinks
alias   ls='ls -F --color=auto --hyperlink' 

alias	ps='ps -ef'
alias	cls='clear; tput reset'
alias 	rm='rm -i'
alias	mv='mv -i'
alias 	cp='cp -i'
alias   more=less
alias   clipin='wl-copy'
alias   clipout='wl-paste'
alias   mailq='msmtp-queue'


###
# FUNCTIONS
###

# Quickly edit executable scripts
vimbin() {
$EDITOR `which $1`
}

alias vi=${EDITOR:-nvim}

# 10ms for key sequences
KEYTIMEOUT=1

###
# LOCAL SETTINGS
###

if [ -f $HOME/.zshrc-$SHORTHOST ]
then
    . $HOME/.zshrc-$SHORTHOST
fi

###
# AUTOCOMPLETION
###

# Load Bash's autocompletion scripts
autoload -U +X bashcompinit && bashcompinit

# Load my autocompletion scripts
if [[ -d ~/.zshcomplete ]]; then
	fpath=(~/.zshcomplete $fpath)
	autoload -U ~/.zshcomplete/*(:t)
fi

# Make for verbose autocompletion hints
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

# Initialize autocompletion
zstyle :compinstall filename '$HOME/.zshrc'
autoload -U compinit
compinit

zstyle ':completion:*' file-sort date

# Fix for TRAMP
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
