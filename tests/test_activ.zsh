#!/usr/bin/env zsh
# activ picks a project's activation script: bin/activate, then Almanack's
# bin/a-activate, then the plain .venv/bin/activate. Each case builds a
# throwaway project whose scripts only set WHICH, and checks which one ran.
#
#   zsh tests/test_activ.zsh
here=${0:A:h}
zshrc=$here/../zsh/.zshrc
fns=$(mktemp -t activ-fns)
sed -n '/^activ() {/,/^}/p' "$zshrc" > "$fns"
root=$(mktemp -d -t activ-test)
trap '\rm -rf "$root" "$fns"' EXIT
fails=0

# mkproj NAME [venv] [bin] [a] -- which scripts to create
mkproj() {
    local d=$root/$1; shift
    mkdir -p "$d/.home/Private/secrets"
    for s in "$@"; do
        case $s in
            venv) mkdir -p "$d/.venv/bin"; echo 'WHICH=venv; deactivate() { :; }' > "$d/.venv/bin/activate" ;;
            bin)  mkdir -p "$d/bin"; echo 'WHICH=bin' > "$d/bin/activate" ;;
            a)    mkdir -p "$d/bin"; echo 'WHICH=a' > "$d/bin/a-activate" ;;
        esac
    done
}

# check NAME EXPECTED_WHICH EXPECTED_STATUS
check() {
    local out
    out=$(cd "$root/$1" && HOME=$root/$1/.home zsh -fc "
        source ${(q)fns}
        activ 2>/dev/null; s=\$?
        print -r -- \"\${WHICH:-unset} \$s\"")
    if [[ $out == "$2 $3" ]]; then
        print "ok: $1 -> $out"
    else
        print "FAIL: $1 -> $out, expected $2 $3"
        fails=1
    fi
}

mkproj proj_bin venv bin
mkproj proj_a venv a
mkproj proj_venv venv
mkproj proj_novenv bin

check proj_bin bin 0
check proj_a a 0
check proj_venv venv 0
check proj_novenv unset 1
exit $fails
