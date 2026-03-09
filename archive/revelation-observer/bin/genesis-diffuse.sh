#!/usr/bin/env sh
# genesis-diffuse.sh - 8-pass structural diffusion via regex
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Purpose: Project any file through 8 regex passes to reveal structural basis
# Constraint: No parsing, no ASTs, no semantic interpretation - only regex
#
# Usage:
#   ./genesis-diffuse.sh <file> [--verbose]
#
# Output:
#   Final basis hash (SHA256 of pass 7)
#   Optional: All intermediate passes (with --verbose)

set -eu

usage() {
    cat >&2 << 'EOF'
Usage: genesis-diffuse.sh <file> [--verbose]

8-Pass Structural Diffusion:
  Pass 0: Identity (anchor)
  Pass 1: Alphanumeric collapse (software projection)
  Pass 2: Non-alphanumeric isolation (syntax projection)
  Pass 3: Boundary normalization
  Pass 4: Repetition compression
  Pass 5: Symmetry check (sort + uniq)
  Pass 6: Context adjacency (sliding window)
  Pass 7: Closure (basis promotion via hash)

Options:
  --verbose    Show all intermediate passes
  --keep-temp  Keep temporary files (in /tmp/genesis-diffuse-*)

Examples:
  ./genesis-diffuse.sh myfile.c
  ./genesis-diffuse.sh --verbose myfile.canasm

Output:
  <basis-hash>  (SHA256 of final pass)
EOF
    exit 2
}

# Check arguments
if [ $# -lt 1 ]; then
    usage
fi

FILE=""
VERBOSE=0
KEEP_TEMP=0

# Parse arguments
for arg in "$@"; do
    case "$arg" in
        --verbose) VERBOSE=1 ;;
        --keep-temp) KEEP_TEMP=1 ;;
        --help) usage ;;
        -*) echo "Unknown option: $arg" >&2; usage ;;
        *) FILE="$arg" ;;
    esac
done

if [ -z "$FILE" ]; then
    echo "Error: No file specified" >&2
    usage
fi

if [ ! -f "$FILE" ]; then
    echo "Error: File not found: $FILE" >&2
    exit 1
fi

# Create temporary workspace
# Use $HOME/tmp if /tmp is not writable (e.g., Android/Termux)
if [ -w /tmp ]; then
    TMP="$(mktemp -d /tmp/genesis-diffuse-XXXXXX)"
else
    mkdir -p "$HOME/tmp"
    TMP="$(mktemp -d "$HOME/tmp/genesis-diffuse-XXXXXX")"
fi

if [ "$KEEP_TEMP" -eq 0 ]; then
    trap 'rm -rf "$TMP"' EXIT
fi

# Copy input file
cp "$FILE" "$TMP/input"

# ============================================================================
# PASS 0: Identity / Anchor
# ============================================================================
# Preserve original file as baseline

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 0: Identity (anchor) ===" >&2
fi

cat "$TMP/input" > "$TMP/pass0"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Length: $(wc -c < "$TMP/pass0") bytes" >&2
    echo "" >&2
fi

# ============================================================================
# PASS 1: Alphanumeric Collapse (Software Projection)
# ============================================================================
# Preserve only alphanumeric characters, replace others with space
# This captures "semantic mass" (P₀ in GENESIS terms)

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 1: Alphanumeric collapse (software projection) ===" >&2
fi

sed 's/[^[:alnum:]]/ /g' "$TMP/pass0" > "$TMP/pass1"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Sample: $(head -c 80 "$TMP/pass1")" >&2
    echo "Length: $(wc -c < "$TMP/pass1") bytes" >&2
    echo "" >&2
fi

# ============================================================================
# PASS 2: Non-Alphanumeric Isolation (Syntax Projection)
# ============================================================================
# Preserve only non-alphanumeric characters (structure)
# This captures "syntax" (P₁ in GENESIS terms)

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 2: Non-alphanumeric isolation (syntax projection) ===" >&2
fi

sed 's/[[:alnum:]]/ /g' "$TMP/pass0" > "$TMP/pass2"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Sample: $(head -c 80 "$TMP/pass2")" >&2
    echo "Length: $(wc -c < "$TMP/pass2") bytes" >&2
    echo "" >&2
fi

# ============================================================================
# PASS 3: Boundary Normalization
# ============================================================================
# Collapse multiple whitespace into single space
# Normalizes boundaries between tokens

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 3: Boundary normalization ===" >&2
fi

sed 's/[[:space:]]\+/ /g' "$TMP/pass2" > "$TMP/pass3"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Sample: $(head -c 80 "$TMP/pass3")" >&2
    echo "Length: $(wc -c < "$TMP/pass3") bytes" >&2
    echo "" >&2
fi

# ============================================================================
# PASS 4: Repetition Compression
# ============================================================================
# Compress repeated characters to single instance
# Example: "aaaa" → "a", "{{{{" → "{"

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 4: Repetition compression ===" >&2
fi

sed 's/\(.\)\1\+/\1/g' "$TMP/pass3" > "$TMP/pass4"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Sample: $(head -c 80 "$TMP/pass4")" >&2
    echo "Length: $(wc -c < "$TMP/pass4") bytes" >&2
    echo "" >&2
fi

# ============================================================================
# PASS 5: Symmetry Check (Line Sort + Unique)
# ============================================================================
# Sort lines and remove duplicates
# This reveals structural symmetries independent of order

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 5: Symmetry check (sort + uniq) ===" >&2
fi

sort "$TMP/pass4" | uniq > "$TMP/pass5"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Unique lines: $(wc -l < "$TMP/pass5")" >&2
    echo "Sample: $(head -3 "$TMP/pass5" | tr '\n' ' ')" >&2
    echo "" >&2
fi

# ============================================================================
# PASS 6: Context Adjacency (Sliding Window)
# ============================================================================
# Insert delimiter between adjacent characters
# Creates bigram view: "abc" → "a|b|c"
# Captures local context without global position

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 6: Context adjacency (sliding window) ===" >&2
fi

sed 's/\(.\)\(.\)/\1|\2/g' "$TMP/pass5" > "$TMP/pass6"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Sample: $(head -c 80 "$TMP/pass6")" >&2
    echo "Length: $(wc -c < "$TMP/pass6") bytes" >&2
    echo "" >&2
fi

# ============================================================================
# PASS 7: Closure / Basis Promotion (Hash)
# ============================================================================
# Final projection to basis: irreversible hash
# This is the "structural fingerprint" - stable, deterministic

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Pass 7: Closure (basis promotion) ===" >&2
fi

# Use SHA256 for basis hash
BASIS=$(sha256sum "$TMP/pass6" | awk '{print $1}')

echo "$BASIS" > "$TMP/pass7"

if [ "$VERBOSE" -eq 1 ]; then
    echo "Basis hash: $BASIS" >&2
    echo "" >&2
fi

# ============================================================================
# Output
# ============================================================================

if [ "$VERBOSE" -eq 1 ]; then
    echo "=== Diffusion Complete ===" >&2
    echo "Input:  $FILE" >&2
    echo "Passes: 8 (0-7)" >&2
    echo "Basis:  $BASIS" >&2

    if [ "$KEEP_TEMP" -eq 1 ]; then
        echo "Temp files: $TMP" >&2
    fi
else
    # Non-verbose: just output the basis hash
    echo "$BASIS"
fi
