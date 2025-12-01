# Hostname detection
declare -x SHORTHOST=${SHORTHOST:=$(hostname -s)}
[[ $SHORTHOST == *.local ]] && SHORTHOST=${SHORTHOST%.local}

###
# Me!
###
declare -x DEBEMAIL=danny@spesh.com
declare -x DEBFULLNAME="Danny O'Brien"
declare -x EMAIL="$DEBFULLNAME <$DEBEMAIL>"

###
# Key bindings
###
# vim-style bindings
bindkey -A viins main
# 10ms for key sequences
KEYTIMEOUT=1

###
# Prompt
###
# If pasted, is null action under zsh so you can copy and paste
declare -x PS1=': %m %1(j.[%j].) %~%#; '

if [[ -n $GUIX_ENVIRONMENT ]]; then
    declare -x PS1=': [] %m %1(j.[%j].) %~%#; '
fi

###
# Shell options
###
umask 002                    # More generous umask
setopt auto_cd               # Allow directories to be typed without 'cd'
setopt auto_pushd            # Always push directories
setopt cdable_vars           # cd to stuff added with 'hash -d'
unsetopt promptcr            # Stops prompt overwriting non-cr'd results

HISTSIZE=100000                   # Lines of history in memory
SAVEHIST=100000                   # Lines saved to file
setopt extended_history          # Save timestamp and duration
setopt hist_verify           # Echo back history substitutions
setopt share_history         # Multiple zsh shells share their history
setopt list_packed           # Compact listings of shell history
setopt hist_ignore_dups      # Ignore repeated commands
setopt hist_reduce_blanks    # Remove superfluous blank lines

###
# Core aliases
###
alias ps='ps -ef'
alias cls='clear; tput reset'
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias more=less
alias mailq='msmtp-queue'
alias vi=${EDITOR:-nvim}
alias llmw='llm --tool Patch --tool Todo --cl 0'

###
# Core functions
###

# Quickly edit executable scripts
vimbin() {
    $EDITOR `which $1`
}

# Create and cd to temp directory
tempe() {
    cd "$(mktemp -d)"
    chmod -R 0700 .
    if [[ $# -eq 1 ]]; then
        \mkdir -p "$1"
        cd "$1"
        chmod -R 0700 .
    fi
}

# Paste into journal or add todo
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

dob-mkindex() {
    local dir="${1:-.}" recursive=false title="" show_hidden=false
    while [[ $# -gt 0 ]]; do
        case $1 in
            (-r|--recursive) recursive=true; shift ;;
            (-t|--title) title="$2"; shift 2 ;;
            (-a|--all) show_hidden=true; shift ;;
            (-h|--help)
                echo "Usage: dob-mkindex [OPTIONS] [DIRECTORY]
Options: -r (recursive), -t TITLE (custom title), -a (include hidden), -h (help)"
                return 0 ;;
            (-*) echo "Unknown option: $1" >&2; return 1 ;;
            (*) dir="$1"; shift ;;
        esac
    done
    dir=$(cd "$dir" && pwd) || { echo "Directory '$dir' not found" >&2; return 1; }
    [[ -z "$title" ]] && title="Index of $(basename "$dir")"

    _get_icon() {
        local filename="$1"
        local icon="📄"
        case "${filename##*.}" in
            (jpg|jpeg|png|gif|svg|webp) icon="🖼️" ;;
            (mp3|wav|ogg|m4a) icon="🎵" ;;
            (mp4|avi|mkv|mov) icon="🎬" ;;
            (pdf) icon="📕" ;;
            (txt|md) icon="📝" ;;
            (zip|tar|gz|rar) icon="📦" ;;
            (html|htm) icon="🌐" ;;
            (js) icon="⚡" ;;
            (py) icon="🐍" ;;
        esac
        echo "$icon"
    }

    _gen_html() {
        local d="$1" t="$2" h="$3"
        local f="$d/index.html"
        local hide_args=()
        [[ "$h" = false ]] && hide_args=(! -name ".*")

        cat > "$f" <<EOF
<!DOCTYPE html><html><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>$t</title><style>body{font-family:system-ui,sans-serif;max-width:800px;margin:2rem auto;padding:0 1rem;color:#333}
h1{color:#2c3e50;border-bottom:2px solid #3498db;padding-bottom:.5rem}.file-list{list-style:none;padding:0}
.file-list li{margin:.5rem 0;padding:.5rem}.file-list a{text-decoration:none;color:#3498db;display:flex;align-items:center}
.file-list a:hover{text-decoration:underline}.icon{margin-right:.5rem}</style></head><body><h1>$t</h1><ul class="file-list">
EOF
        [[ "$d" != "/" ]] && echo '<li><a href="../"><span class="icon">↩️</span>../</a></li>' >> "$f"

        find -L "$d" -maxdepth 1 -type d ! -path "$d" "${hide_args[@]}" -print0 2>/dev/null | sort -z | while IFS= read -r -d '' item; do
            local bn=$(basename "$item")
            [[ "$bn" != "." && "$bn" != ".." ]] && echo "<li><a href=\"$bn/\"><span class=\"icon\">📁</span>$bn/</a></li>" >> "$f"
        done

        find -L "$d" -maxdepth 1 -type f "${hide_args[@]}" -print0 2>/dev/null | sort -z | while IFS= read -r -d '' item; do
            local bn=$(basename "$item")
            if [[ "$bn" != "index.html" ]]; then
                local icon=$(_get_icon "$bn")
                echo "<li><a href=\"$bn\"><span class=\"icon\">$icon</span>$bn</a></li>" >> "$f"
            fi
        done

        echo "</ul><div style=\"text-align:center;margin-top:2rem;color:#666;font-size:.9em\">Generated $(date)</div></body></html>" >> "$f"
        echo "Generated: $f"
    }

    _gen_html "$dir" "$title" "$show_hidden"

    if [[ "$recursive" == true ]]; then
        local path_filter=()
        [[ "$show_hidden" = false ]] && path_filter=(! -path "*/.*")
        find -L "$dir" -type d ! -path "$dir" "${path_filter[@]}" -print0 2>/dev/null | while IFS= read -r -d '' subdir; do
            _gen_html "$subdir" "Index of $(basename "$subdir")" "$show_hidden"
        done
    fi
}

dob-tz() {
    local input_time="$1"
    local custom_tz="$2"
    local epoch base_date

    if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
        echo "Usage: dob-tz [TIME] [TIMEZONE] - Convert time to Local/Eastern/UTC/CET/Custom"
        return 0
    fi

    find_timezone() {
        local search_term="$1"
        local tz_dirs=("/usr/share/zoneinfo" "/System/Library/TimeZones")

        for dir in $tz_dirs; do
            [[ -d "$dir" ]] || continue
            if TZ="$search_term" date >/dev/null 2>&1; then
                echo "$search_term"
                return 0
            fi
        done

        for dir in $tz_dirs; do
            [[ -d "$dir" ]] || continue
            local matches=($(find "$dir" -type f -path "*${search_term}*" 2>/dev/null | head -5))
            for match in $matches; do
                local tz_name=${match#$dir/}
                if TZ="$tz_name" date >/dev/null 2>&1; then
                    echo "$tz_name"
                    return 0
                fi
            done
        done

        local -a common_timezones=(
            "Asia/Tokyo" "Asia/Shanghai" "Asia/Hong_Kong" "Asia/Singapore" "Asia/Seoul"
            "Europe/London" "Europe/Paris" "Europe/Berlin" "Europe/Rome" "Europe/Madrid"
            "America/New_York" "America/Los_Angeles" "America/Chicago" "America/Denver"
            "Australia/Sydney" "Australia/Melbourne" "Pacific/Auckland" "America/Toronto"
            "Asia/Kolkata" "Asia/Dubai" "Asia/Bangkok" "Europe/Amsterdam" "Europe/Stockholm"
        )

        for tz in $common_timezones; do
            if [[ "$tz" == *"${search_term}"* ]] || [[ "${tz:l}" == *"${search_term:l}"* ]]; then
                echo "$tz"
                return 0
            fi
        done

        return 1
    }

    if [[ -z "$input_time" ]]; then
        epoch=$(date +%s)
    else
        if [[ "$input_time" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}[[:space:]][0-9]{1,2}:[0-9]{2}$ ]]; then
            base_date="$input_time:00"
        elif [[ "$input_time" =~ "AM|PM|am|pm" ]]; then
            base_date=$(date -j -f "%I:%M %p" "$input_time" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || date -d "$input_time" 2>/dev/null)
        else
            base_date=$(date -j -f "%H:%M" "$input_time" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || date -d "today $input_time" 2>/dev/null)
        fi

        if [[ -z "$base_date" ]]; then
            echo "Error: Could not parse time '$input_time'"
            return 1
        fi

        if command -v gdate >/dev/null 2>&1; then
            epoch=$(gdate -d "$base_date" +%s 2>/dev/null)
        elif date --version 2>&1 | grep -q GNU; then
            epoch=$(date -d "$base_date" +%s 2>/dev/null)
        else
            epoch=$(date -j -f "%a %b %d %H:%M:%S %Z %Y" "$base_date" +%s 2>/dev/null || date +%s)
        fi
    fi

    local local_date=$(date -r "$epoch" +%Y-%m-%d)
    local eastern_date=$(TZ="America/New_York" date -r "$epoch" +%Y-%m-%d)
    local utc_date=$(TZ="UTC" date -r "$epoch" +%Y-%m-%d)
    local cet_date=$(TZ="Europe/Berlin" date -r "$epoch" +%Y-%m-%d)

    local local_time=$(date -r "$epoch" "+$local_date %H:%M %Z")
    local eastern_time=$(TZ="America/New_York" date -r "$epoch" "+%H:%M %Z")
    [[ "$eastern_date" != "$local_date" ]] && eastern_time="$eastern_date $eastern_time"
    local utc_time=$(TZ="UTC" date -r "$epoch" "+%H:%M %Z")
    [[ "$utc_date" != "$local_date" ]] && utc_time="$utc_date $utc_time"
    local cet_time=$(TZ="Europe/Berlin" date -r "$epoch" "+%H:%M %Z")
    [[ "$cet_date" != "$local_date" ]] && cet_time="$cet_date $cet_time"

    local output="$local_time | Eastern: $eastern_time | UTC: $utc_time | CET: $cet_time"

    if [[ -n "$custom_tz" ]]; then
        local found_tz=$(find_timezone "$custom_tz")
        if [[ -n "$found_tz" ]]; then
            local custom_date=$(TZ="$found_tz" date -r "$epoch" +%Y-%m-%d 2>/dev/null)
            if [[ $? -eq 0 ]]; then
                local custom_time=$(TZ="$found_tz" date -r "$epoch" "+%H:%M %Z")
                [[ "$custom_date" != "$local_date" ]] && custom_time="$custom_date $custom_time"
                local tz_label=$(echo "$found_tz" | sed 's/.*\///')
                [[ "$found_tz" != "$custom_tz" ]] && echo "Found timezone: $found_tz" >&2
                output="$output | $tz_label: $custom_time"
            fi
        else
            echo "Error: No timezone found matching '$custom_tz'"
            return 1
        fi
    fi

    echo "$output"
}

dob-cid() {
    [[ -z "$1" ]] && { echo "Usage: cid_convert <CID>"; return 1; }
    command -v ipfs >/dev/null || { echo "Error: ipfs not found"; return 1; }

    if [[ "$1" =~ ^Qm[1-9A-HJ-NP-Za-km-z]{44}$ ]]; then
        ipfs cid format -v 1 -b base32 "$1"
    else
        ipfs cid format -v 0 "$1" 2>/dev/null || echo "Error: Conversion failed"
    fi
}

dob-spelling() {
    local template_file="$(llm templates path)/spelling.yml"

    if [ ! -e "$template_file" ]; then
        echo "Downloading spelling template..."
        if ! curl -s 'https://raw.githubusercontent.com/dannyob/llm-templates/refs/heads/main/spelling.yaml' > "$template_file"; then
            echo "Error: Failed to download template" >&2
            return 1
        fi
    fi

    llm -t spelling --cl 0 -p filename "$@"
}

###
# Python cache
###
case "${OSTYPE}" in
    darwin*)
        folder="${HOME}/Library/Caches/Python"
        if [[ ! -d "${folder}" ]]; then mkdir -p "${folder}"; fi
        export PYTHONPYCACHEPREFIX="${folder}"
        ;;
    linux*)
        folder="${HOME}/.cache/Python"
        if [[ ! -d "${folder}" ]]; then mkdir -p "${folder}"; fi
        export PYTHONPYCACHEPREFIX="${folder}"
        ;;
esac

###
# Platform-specific configuration
###
if [[ "$OSTYPE" == darwin* ]]; then
    [[ -f ~/.zshrc_macos ]] && source ~/.zshrc_macos
else
    [[ -f ~/.zshrc_linux ]] && source ~/.zshrc_linux
fi

###
# GPG/SSH agent setup (after platform config so gpgconf is in PATH)
###
if command -v gpgconf >/dev/null 2>&1; then
    gpg-connect-agent /bye >/dev/null 2>&1
    GPG_TTY=$(tty)
    export GPG_TTY

    unset SSH_AGENT_PID
    # Only use GPG agent if not using forwarded SSH agent
    if [[ -z "$SSH_AUTH_SOCK" ]] || [[ "$SSH_AUTH_SOCK" == *"gpg"* ]]; then
        export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    fi
    echo UPDATESTARTUPTTY | gpg-connect-agent > /dev/null 2>&1
fi

###
# Guix as package manager (on any platform)
###
if command -v guix >/dev/null 2>&1; then
    export GUIX_LOCPATH=${GUIX_LOCPATH:-$HOME/.guix-profile/lib/locale}
    fpath=(~/.config/guix/current/share/zsh/site-functions/ "${fpath[@]}")

    guixinstall() {
        local commit
        vared -c -p "Why are you installing $1? " commit
        if guix package -i $1; then
            cat >>~/ChangeLog<<EOF

$(date -Imin)  $DEBFULLNAME  <$DEBEMAIL>

	* $commit

	% guix package -i $1
EOF
        else
            echo "Not logged!"
        fi
    }

    guixupgrade() {
        (
            cd ~/Public/src/guix/
            git pull
            guix pull --disable-authentication --allow-downgrades
            donotupgrade=$(guix package --upgrade --dry-run |& awk '/following derivation/{flag=1;next} /would be downloaded/{flag=0;}  flag { sub(/^[^-]*-/,"") ; sub("-[0-9].*$",""); print }' | tr '\n' ' ')
            echo guix package -u $(echo $donotupgrade | sed 's/^\| \([^$]\)/ --do-not-upgrade \1/g')
            guix package $(echo $donotupgrade | sed 's/^\| \([^$]\)/ --do-not-upgrade \1/g') --upgrade
        )
    }
fi

###
# PLUGINS (zsh_unplugged)
###
ZPLUGINDIR=${ZPLUGINDIR:-${HOME}/.config/zsh/plugins}

function plugin-load {
    local repo plugdir initfile initfiles=()
    for repo in $@; do
        plugdir=$ZPLUGINDIR/${repo:t}
        initfile=$plugdir/${repo:t}.plugin.zsh
        if [[ ! -d $plugdir ]]; then
            echo "Cloning $repo..."
            git clone -q --depth 1 https://github.com/$repo $plugdir
        fi
        if [[ ! -e $initfile ]]; then
            initfiles=($plugdir/*.{plugin.zsh,zsh-theme,zsh,sh}(N))
            (( $#initfiles )) || continue
            ln -sf $initfiles[1] $initfile
        fi
        fpath+=$plugdir
        . $initfile
    done
}

repos=(
    zsh-users/zsh-syntax-highlighting
    zsh-users/zsh-history-substring-search
    zsh-users/zsh-completions
)
plugin-load $repos

# fasd
if [[ -x ~/.local/bin/fasd ]]; then
    eval "$(~/.local/bin/fasd --init auto)"
    bindkey '^X^A' fasd-complete
    bindkey '^X^F' fasd-complete-f
    bindkey '^X^D' fasd-complete-d
    alias c='fasd_cd -d'
    alias cc='fasd_cd -d -i'
    alias v='f -e ${EDITOR:-nvim}'
fi

# history-substring-search keybindings
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
zmodload zsh/terminfo
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

###
# Machine-specific configuration
###
if [[ -f $HOME/.zshrc-$SHORTHOST ]]; then
    source $HOME/.zshrc-$SHORTHOST
fi

###
# Misc
###

# Fix for TRAMP
[[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ '

# Wasmer
export WASMER_DIR="$HOME/.wasmer"
[[ -s "$WASMER_DIR/wasmer.sh" ]] && source "$WASMER_DIR/wasmer.sh"

# Video summarization directory
[[ -d "$HOME/Public/dannyob.eth/video/sumvideo" ]] && export SUMVIDEO_DIR=$HOME/Public/dannyob.eth/video/sumvideo/

export PATH="$HOME/.local/bin:$PATH"
