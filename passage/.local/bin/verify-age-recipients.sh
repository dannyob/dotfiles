#!/usr/bin/env bash
#
# verify-age-recipients.sh - Verify .age-recipients changes are properly signed
#
# Usage: verify-age-recipients.sh [--since COMMIT] [STORE_DIR]
#
# Verifies that all commits modifying .age-recipients are signed by an SSH key
# whose age-equivalent was already in .age-recipients at the time of signing.
#
# Requirements: git, ssh-to-age, ssh-keygen
#
# Exit codes:
#   0 - All changes verified (safe to reencrypt)
#   1 - Verification failed (DO NOT reencrypt)
#   2 - Missing dependencies or invalid arguments

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

die() {
    echo -e "${RED}ERROR: $1${NC}" >&2
    exit "${2:-1}"
}

warn() {
    echo -e "${YELLOW}WARNING: $1${NC}" >&2
}

info() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

fail() {
    echo -e "${RED}[FAIL]${NC} $1"
}

debug() {
    if [[ "${VERIFY_AGE_DEBUG:-0}" == "1" ]]; then
        echo -e "${BLUE}[DEBUG]${NC} $1" >&2
    fi
}

# Check dependencies
check_deps() {
    local missing=()
    command -v git >/dev/null || missing+=(git)
    command -v ssh-to-age >/dev/null || missing+=(ssh-to-age)
    command -v ssh-keygen >/dev/null || missing+=(ssh-keygen)

    if [[ ${#missing[@]} -gt 0 ]]; then
        die "Missing dependencies: ${missing[*]}" 2
    fi
}

# Check if a commit has a valid SSH signature
# Returns 0 if signature is valid, 1 otherwise
# Note: We check for "Good" in output, not exit code, because git verify-commit
# fails when no principal matches even if the signature is cryptographically valid
has_valid_signature() {
    local commit="$1"
    local verify_output
    verify_output=$(git verify-commit "$commit" 2>&1 || true)
    debug "verify-commit output: $verify_output"

    if echo "$verify_output" | grep -q 'Good "git" signature'; then
        return 0
    else
        return 1
    fi
}

# Extract SSH public key from a commit signature and convert to age recipient
# The SSH signature format (SSHSIG):
#   - Bytes 0-5: "SSHSIG" magic
#   - Bytes 6-9: version (uint32)
#   - Bytes 10-13: public key blob length (uint32)
#   - Bytes 14+: public key blob
# For ed25519, the pubkey blob is 51 bytes:
#   - 4 bytes: key type length (0x0000000b = 11)
#   - 11 bytes: "ssh-ed25519"
#   - 4 bytes: key data length (0x00000020 = 32)
#   - 32 bytes: ed25519 public key
get_signer_age_key() {
    local commit="$1"

    # Get the signature from the commit
    local sig
    sig=$(git cat-file commit "$commit" | \
          sed -n '/^gpgsig /,/-----END SSH SIGNATURE-----/p' | \
          sed 's/^gpgsig //' | sed 's/^ //')

    if [[ -z "$sig" ]]; then
        debug "No signature found in commit $commit"
        echo ""
        return
    fi

    # Extract base64-encoded signature body
    local sig_b64
    sig_b64=$(echo "$sig" | grep -v '^-----' | tr -d '\n ')

    if [[ -z "$sig_b64" ]]; then
        debug "Could not extract base64 signature body"
        echo ""
        return
    fi

    # Decode signature and extract public key blob (51 bytes at offset 14)
    # Then convert to SSH authorized_keys format and to age
    local pubkey_b64
    pubkey_b64=$(echo "$sig_b64" | base64 -d 2>/dev/null | \
                 dd bs=1 skip=14 count=51 2>/dev/null | base64)

    if [[ -z "$pubkey_b64" ]]; then
        debug "Could not extract pubkey blob from signature"
        echo ""
        return
    fi

    debug "Extracted pubkey blob: ssh-ed25519 $pubkey_b64"

    # Convert to age format
    local age_key
    age_key=$(echo "ssh-ed25519 $pubkey_b64" | ssh-to-age 2>/dev/null)

    if [[ -z "$age_key" ]]; then
        debug "ssh-to-age conversion failed"
        echo ""
        return
    fi

    echo "$age_key"
}

# Get .age-recipients content at a specific commit
# Filters out comments and empty lines
get_recipients_at_commit() {
    local commit="$1"
    git show "${commit}:.age-recipients" 2>/dev/null | grep -v '^#' | grep -v '^$' || true
}

# Get parent commit hash
get_parent() {
    local commit="$1"
    git rev-parse "${commit}^" 2>/dev/null || echo ""
}

# Check if this is the first commit that creates .age-recipients (bootstrap)
is_bootstrap_commit() {
    local commit="$1"
    local parent
    parent=$(get_parent "$commit")

    if [[ -z "$parent" ]]; then
        # No parent - this is the first commit in the repo
        return 0
    fi

    # Check if .age-recipients existed in parent
    if git show "${parent}:.age-recipients" >/dev/null 2>&1; then
        return 1  # File existed in parent, not bootstrap
    else
        return 0  # File did not exist in parent, this is bootstrap
    fi
}

# Main verification logic
verify_recipients_changes() {
    local store_dir="$1"
    local since_commit="${2:-}"

    cd "$store_dir" || die "Cannot access store directory: $store_dir"

    # Check we're in a git repo
    git rev-parse --git-dir >/dev/null 2>&1 || die "Not a git repository"

    # Check .age-recipients exists
    [[ -f .age-recipients ]] || die ".age-recipients not found"

    # Find commits that modified .age-recipients
    local commits
    if [[ -n "$since_commit" ]]; then
        commits=$(git log --format='%H' --reverse "${since_commit}..HEAD" -- .age-recipients 2>/dev/null || true)
    else
        commits=$(git log --format='%H' --reverse -- .age-recipients 2>/dev/null || true)
    fi

    if [[ -z "$commits" ]]; then
        info "No changes to .age-recipients to verify"
        return 0
    fi

    echo "Verifying commits that modified .age-recipients..."
    echo ""

    local failed=0
    local commit_count=0
    local verified_count=0

    while IFS= read -r commit; do
        [[ -z "$commit" ]] && continue
        ((commit_count++))

        local short_commit="${commit:0:8}"
        local subject
        subject=$(git log -1 --format='%s' "$commit")
        local author
        author=$(git log -1 --format='%an <%ae>' "$commit")
        local date
        date=$(git log -1 --format='%ci' "$commit")

        echo "Commit: $short_commit"
        echo "  Subject: $subject"
        echo "  Author:  $author"
        echo "  Date:    $date"

        # Check if commit has a valid signature
        if ! has_valid_signature "$commit"; then
            fail "  Commit is NOT signed or signature invalid"
            failed=1
            echo ""
            continue
        fi

        # Get signer's age key
        local signer_age_key
        signer_age_key=$(get_signer_age_key "$commit")

        if [[ -z "$signer_age_key" ]]; then
            fail "  Could not extract signer's age key from signature"
            failed=1
            echo ""
            continue
        fi

        echo "  Signer's age key: $signer_age_key"

        # Determine if this is a bootstrap commit
        if is_bootstrap_commit "$commit"; then
            # BOOTSTRAP CASE: First .age-recipients commit
            # Rule: Signer's age key must be IN the new .age-recipients (self-consistent)
            local current_recipients
            current_recipients=$(get_recipients_at_commit "$commit")

            if echo "$current_recipients" | grep -qF "$signer_age_key"; then
                info "  Bootstrap commit - signer included themselves"
                ((verified_count++))
            else
                fail "  INVALID BOOTSTRAP: Signer's key is NOT in their own .age-recipients"
                echo "    A valid bootstrap requires the signer to include their own age key."
                failed=1
            fi
        else
            # NORMAL CASE: Check if signer's age key was in PREVIOUS recipients
            local parent
            parent=$(get_parent "$commit")
            local previous_recipients
            previous_recipients=$(get_recipients_at_commit "$parent")

            if echo "$previous_recipients" | grep -qF "$signer_age_key"; then
                info "  Signer was authorized (key in previous .age-recipients)"
                ((verified_count++))
            else
                fail "  UNAUTHORIZED: Signer's key was NOT in previous .age-recipients"
                echo "    This commit may be an attempt to add an unauthorized key!"
                failed=1
            fi
        fi

        echo ""
    done <<< "$commits"

    echo "Summary: $verified_count/$commit_count commits verified"
    return $failed
}

# Parse arguments
SINCE_COMMIT=""
STORE_DIR="${PASSWORD_STORE_DIR:-$HOME/.password-store}"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --since)
            SINCE_COMMIT="$2"
            shift 2
            ;;
        --debug)
            export VERIFY_AGE_DEBUG=1
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--since COMMIT] [--debug] [STORE_DIR]"
            echo ""
            echo "Verify .age-recipients changes are properly signed before reencrypting."
            echo ""
            echo "This script checks that every commit modifying .age-recipients is signed"
            echo "by someone whose age key was already authorized at the time of signing."
            echo ""
            echo "Options:"
            echo "  --since COMMIT   Only check commits after COMMIT"
            echo "  --debug          Show debug output"
            echo "  STORE_DIR        Password store directory (default: \$PASSWORD_STORE_DIR or ~/.password-store)"
            echo ""
            echo "Authorization rules:"
            echo "  - Bootstrap commits: Signer must include their own age key"
            echo "  - Subsequent commits: Signer must be in PREVIOUS .age-recipients"
            echo ""
            echo "Exit codes:"
            echo "  0 - All changes verified (safe to reencrypt)"
            echo "  1 - Verification failed (DO NOT reencrypt)"
            echo "  2 - Missing dependencies or invalid arguments"
            exit 0
            ;;
        -*)
            die "Unknown option: $1" 2
            ;;
        *)
            STORE_DIR="$1"
            shift
            ;;
    esac
done

# Main
check_deps

echo "========================================"
echo " .age-recipients Verification"
echo "========================================"
echo ""
echo "Store: $STORE_DIR"
echo ""

if verify_recipients_changes "$STORE_DIR" "$SINCE_COMMIT"; then
    echo ""
    echo -e "${GREEN}========================================"
    echo " All changes verified - safe to reencrypt"
    echo "========================================${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}========================================"
    echo " VERIFICATION FAILED"
    echo ""
    echo " DO NOT run 'passage reencrypt' until you"
    echo " manually verify the unauthorized changes!"
    echo "========================================${NC}"
    exit 1
fi
