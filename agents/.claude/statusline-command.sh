#!/bin/sh
# Status line based on Danny's zsh PS1: ': %m %1(j.[%j].) %~%#; '
# Original PS1 structure: : <host> [jobs] <cwd>

input=$(cat)
cwd=$(echo "$input" | jq -r '.cwd // .workspace.current_dir // empty')

# Shorten home directory to ~
home="$HOME"
if [ -n "$cwd" ]; then
    short_cwd="${cwd/#$home/~}"
else
    short_cwd="$(pwd)"
    short_cwd="${short_cwd/#$home/~}"
fi

host=$(hostname -s)
# Strip .local suffix if present
host="${host%.local}"

printf ': %s %s' "$host" "$short_cwd"
