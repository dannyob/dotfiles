# last chance
declare -x SHORTHOST=${SHORTHOST:=$(hostname -s)}
[[ $SHORTHOST == *.local ]] && SHORTHOST=${SHORTHOST%.local}

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
alias   ls='ls -A -F --group-directories-first --sort=extension --color=auto --hyperlink' 

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

shellit() {
    llm -t shellit $* | awk '/```/ {flag=1;next} /```/ {flag=0} flag'
}

function »() {
zmodload zsh/zselect
local fds=(0)

    # Run paste-into-journal if there is something on stdin OR if there are no arguments
    if zselect -t 0 $fds 2> /dev/null || [[ $# -eq 0 ]]; then
        paste-into-journal
    else
        emacsclient -e "(progn (dob-add-journal-todo)(insert \"$*\")(save-buffer))"
    fi
}

tidyhtml() {
    pandoc -f html-native_divs-native_spans -t markdown-raw_html-raw_attribute | pandoc -f markdown -t html
}

html2markdown() {
    pandoc -f html-native_divs-native_spans -t markdown-raw_html-raw_attribute 
}

fixgpg() {
    gpg-connect-agent /bye
    GPG_TTY=$(tty)
    export GPG_TTY
}

dob-mkindex () {
	local dir="${1:-.}" recursive=false title="" show_hidden=false 
	while [[ $# -gt 0 ]]
	do
		case $1 in
			(-r|--recursive) recursive=true 
				shift ;;
			(-t|--title) title="$2" 
				shift 2 ;;
			(-a|--all) show_hidden=true 
				shift ;;
			(-h|--help) echo "Usage: dob-mkindex [OPTIONS] [DIRECTORY]
Options: -r (recursive), -t TITLE (custom title), -a (include hidden), -h (help)"
				return 0 ;;
			(-*) echo "Unknown option: $1" >&2
				return 1 ;;
			(*) dir="$1" 
				shift ;;
		esac
	done
	dir=$(cd "$dir" && pwd)  || {
		echo "Directory '$dir' not found" >&2
		return 1
	}
	[[ -z "$title" ]] && title="Index of $(basename "$dir")" 
	_gen_html () {
		local d="$1" 
		local t="$2" 
		local h="$3" 
		local f="$d/index.html" 
		cat > "$f" <<EOF
<!DOCTYPE html><html><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>$t</title><style>body{font-family:system-ui,sans-serif;max-width:800px;margin:2rem auto;padding:0 1rem;color:#333}
h1{color:#2c3e50;border-bottom:2px solid #3498db;padding-bottom:.5rem}.file-list{list-style:none;padding:0}
.file-list li{margin:.5rem 0;padding:.5rem}.file-list a{text-decoration:none;color:#3498db;display:flex;align-items:center}
.file-list a:hover{text-decoration:underline}.icon{margin-right:.5rem}</style></head><body><h1>$t</h1><ul class="file-list">
EOF
		[[ "$d" != "/" ]] && echo '<li><a href="../"><span class="icon">↩️</span>../</a></li>' >> "$f"
		find "$d" -maxdepth 1 -type d ! -path "$d" $([ "$h" = false ] && echo "! -name '.*'") -print0 2> /dev/null | sort -z | while IFS= read -r -d '' item
		do
			local bn=$(basename "$item") 
			[[ "$bn" != "." && "$bn" != ".." ]] && echo "<li><a href=\"$bn/\"><span class=\"icon\">📁</span>$bn/</a></li>" >> "$f"
		done
		find "$d" -maxdepth 1 -type f $([ "$h" = false ] && echo "! -name '.*'") -print0 2> /dev/null | sort -z | while IFS= read -r -d '' item
		do
			local bn=$(basename "$item") 
			if [[ "$bn" != "index.html" ]]
			then
				local icon="📄" 
				case "${bn##*.}" in
					(jpg|jpeg|png|gif|svg|webp) icon="🖼️"  ;;
					(mp3|wav|ogg|m4a) icon="🎵"  ;;
					(mp4|avi|mkv|mov) icon="🎬"  ;;
					(pdf) icon="📕"  ;;
					(txt|md) icon="📝"  ;;
					(zip|tar|gz|rar) icon="📦"  ;;
					(html|htm) icon="🌐"  ;;
					(js) icon="⚡"  ;;
					(py) icon="🐍"  ;;
				esac
				echo "<li><a href=\"$bn\"><span class=\"icon\">$icon</span>$bn</a></li>" >> "$f"
			fi
		done
		echo "</ul><div style=\"text-align:center;margin-top:2rem;color:#666;font-size:.9em\">Generated $(date)</div></body></html>" >> "$f"
		echo "Generated: $f"
	}
	_gen_html "$dir" "$title" "$show_hidden"
	if [[ "$recursive" == true ]]
	then
		find "$dir" -type d ! -path "$dir" $([ "$show_hidden" = false ] && echo "! -path '*/.*'") -print0 2> /dev/null | while IFS= read -r -d '' subdir
		do
			_gen_html "$subdir" "Index of $(basename "$subdir")" "$show_hidden"
		done
	fi
}


# Stop Python Cache droppings
case "${OSTYPE}" in
    darwin*)
        folder="${HOME}/Library/Caches/Python"
        if [[ ! -d "${folder}" ]]; then mkdir -p "${folder}"; fi
        export PYTHONPYCACHEPREFIX="${folder}"
        ;;
    linux-*)
        folder="${HOME}/.cache/Python"
        if [[ ! -d "${folder}" ]]; then mkdir -p "${folder}"; fi
        export PYTHONPYCACHEPREFIX="${folder}"
        ;;
    *)
        printf "WARNING: unsupported operating system '%s'; "`
              `'not setting PYTHONPYCACHEPREFIX' "${OSTYPE}" >&2
        return 1
        ;;
esac

# 10ms for key sequences
KEYTIMEOUT=1

###
# LOCAL SETTINGS
###

# BEGIN-INSERT-OTHER-ZSHRC
if [ -f $HOME/.zshrc-$SHORTHOST ]
then
    . $HOME/.zshrc-$SHORTHOST
fi
# END-INSERT-OTHER-ZSHRC

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
