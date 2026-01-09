#!/usr/bin/env sh
# atom-identity-gen.sh - Generate economic identity atom from genesis signature
# Usage: ./atom-identity-gen.sh <identity.json>
set -eu

die() { printf "atom-identity-gen: %s\n" "$*" >&2; exit 1; }

[ $# -eq 1 ] || die "usage: $0 <identity.json>"
IDENTITY_FILE="$1"
[ -f "$IDENTITY_FILE" ] || die "file not found: $IDENTITY_FILE"

# Path to genesis-enhanced.sh (adjust if needed)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GENESIS="${GENESIS:-$SCRIPT_DIR/../tetragrammatron-os_bootstrap_A_then_B/genesis-enhanced.sh}"
[ -x "$GENESIS" ] || die "genesis-enhanced.sh not found or not executable: $GENESIS"

# Extract identity name from JSON (simple grep, assumes "name" field)
NAME=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$IDENTITY_FILE" | \
       sed 's/.*"\([^"]*\)".*/\1/' | tr '[:upper:]' '[:lower:]' | tr -cd '[:alnum:]')
[ -n "$NAME" ] || die "could not extract 'name' field from JSON"

# Run genesis probe
GENESIS_SIG=$($GENESIS translate "$IDENTITY_FILE" 2>/dev/null) || die "genesis probe failed"

# Extract V8 tuple (format: "HW=... V8=...")
V8_SIG=$(echo "$GENESIS_SIG" | head -1 | sed 's/.*V8=//' | cut -d' ' -f1)
[ -n "$V8_SIG" ] || die "could not extract V8 signature"

# Create deterministic atom ID (first 16 chars of sha256)
ATOM_ID=$(printf "%s" "$V8_SIG" | sha256sum | cut -d' ' -f1 | head -c16)

# Output CANASM atom definition
cat <<EOF
; ========================================
; Economic Identity Atom
; Generated from: $IDENTITY_FILE
; ========================================
; Name: $NAME
; V8 Signature: $V8_SIG
; Atom ID: identity:$NAME:$ATOM_ID
;
; Add this to your @atoms section:
identity:$NAME:$ATOM_ID

; Metadata (for reference, not part of atom table):
; {
;   "type": "identity",
;   "name": "$NAME",
;   "v8_signature": "$V8_SIG",
;   "atom_id": "identity:$NAME:$ATOM_ID",
;   "source_file": "$IDENTITY_FILE"
; }
EOF
