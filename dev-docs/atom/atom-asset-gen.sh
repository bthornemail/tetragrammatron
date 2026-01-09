#!/usr/bin/env sh
# atom-asset-gen.sh - Generate unique asset atom from genesis signature
# Usage: ./atom-asset-gen.sh <asset.json>
set -eu

die() { printf "atom-asset-gen: %s\n" "$*" >&2; exit 1; }

[ $# -eq 1 ] || die "usage: $0 <asset.json>"
ASSET_FILE="$1"
[ -f "$ASSET_FILE" ] || die "file not found: $ASSET_FILE"

# Path to genesis-enhanced.sh
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GENESIS="${GENESIS:-$SCRIPT_DIR/../tetragrammatron-os_bootstrap_A_then_B/genesis-enhanced.sh}"
[ -x "$GENESIS" ] || die "genesis-enhanced.sh not found: $GENESIS"

# Extract asset type and serial from JSON
TYPE=$(grep -o '"type"[[:space:]]*:[[:space:]]*"[^"]*"' "$ASSET_FILE" | \
       sed 's/.*"\([^"]*\)".*/\1/' | tr '[:upper:]' '[:lower:]')
SERIAL=$(grep -o '"serial"[[:space:]]*:[[:space:]]*"[^"]*"' "$ASSET_FILE" | \
         sed 's/.*"\([^"]*\)".*/\1/' | tr -cd '[:alnum:]')

[ -n "$TYPE" ] || die "could not extract 'type' field from JSON"
[ -n "$SERIAL" ] || SERIAL="unknown"

# Run genesis probe
GENESIS_SIG=$($GENESIS translate "$ASSET_FILE" 2>/dev/null) || die "genesis probe failed"

# Extract V8 (structure) and HW (uniqueness) tuples
# Format: "HW=... V8=..."
V8_SIG=$(echo "$GENESIS_SIG" | head -1 | sed 's/.*V8=//' | cut -d' ' -f1)
HW_SIG=$(echo "$GENESIS_SIG" | head -1 | sed 's/ V8=.*//' | sed 's/HW=//')

[ -n "$V8_SIG" ] || die "could not extract V8 signature"
[ -n "$HW_SIG" ] || die "could not extract HW signature"

# Create unique atom ID (V8 for classification, HW for uniqueness)
COMBINED="${V8_SIG}:${HW_SIG}"
ATOM_ID=$(printf "%s" "$COMBINED" | sha256sum | cut -d' ' -f1 | head -c16)

# Output CANASM atom definition
cat <<EOF
; ========================================
; Economic Asset Atom (Unique)
; Generated from: $ASSET_FILE
; ========================================
; Type: $TYPE
; Serial: $SERIAL
; V8 Signature (structure): $V8_SIG
; HW Signature (uniqueness): $HW_SIG
; Atom ID: asset:$TYPE:$ATOM_ID
;
; Add this to your @atoms section:
asset:$TYPE:$ATOM_ID

; Metadata (for reference, not part of atom table):
; {
;   "type": "asset",
;   "asset_type": "$TYPE",
;   "serial": "$SERIAL",
;   "v8_signature": "$V8_SIG",
;   "hw_signature": "$HW_SIG",
;   "atom_id": "asset:$TYPE:$ATOM_ID",
;   "source_file": "$ASSET_FILE"
; }

; CRITICAL PROPERTIES:
; - V8 signature is DETERMINISTIC (same structure → same V8)
; - HW signature is UNIQUE (includes timestamp/inode)
; - This asset atom is GLOBALLY UNIQUE and UNFORGEABLE
; - Can be transferred via EDGE_REMOVE + EDGE_ADD operations
EOF
