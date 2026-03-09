#!/bin/sh
# canb-fano-validator.sh - Fano-gate CANB validator
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Validates that a CANB program's atom set satisfies Fano plane closure.
# The Fano plane (7 points, 7 lines) is the multiplication table of octonions.
#
# Usage:
#   ./canb-fano-validator.sh <program.canb>
#
# Output:
#   VALID: <residue-set>    - Atoms form closed Fano subset
#   INVALID: <reason>       - Atoms violate closure constraints

set -e

# Check arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 <program.canb>" >&2
    exit 2
fi

CANB_FILE="$1"

# Find disassembler (relative to this script or in PATH)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DISASM="${SCRIPT_DIR}/../../tetragrammatron-os/bin/canasm0-disasm"

if [ ! -x "$DISASM" ]; then
    echo "ERROR: canasm0-disasm not found at $DISASM" >&2
    echo "       Please build tetragrammatron-os first (make)" >&2
    exit 1
fi

if [ ! -f "$CANB_FILE" ]; then
    echo "ERROR: File not found: $CANB_FILE" >&2
    exit 1
fi

# Fano plane structure (7 lines, each with 3 points)
# Points: 0=e1, 1=e2, 2=e3, 3=e4, 4=e5, 5=e6, 6=e7
# (Note: e0 is the real unit / identity - not explicitly in lines)
FANO_LINES="
0,1,2
0,3,4
0,5,6
1,3,6
1,4,5
2,3,5
2,4,6
"

# Canonical 8-tuple mapping (atoms → octonion units)
# e0 = identity (always present implicitly)
# e1-e7 = imaginary units
CANONICAL_ATOMS="accept alphabet left right delta start state reject"

# Extract atoms from CANB file using disassembler
extract_atoms() {
    "$DISASM" "$CANB_FILE" 2>/dev/null | awk '
        BEGIN { in_atoms = 0; }
        /@atoms/ { in_atoms = 1; next; }
        /@end/ { in_atoms = 0; }
        in_atoms && /^  / {
            # Remove leading spaces and print atom name
            gsub(/^  /, "");
            print;
        }
    '
}

# Map atom name to Fano point index (0-6 for e1-e7, -1 for unknown)
atom_to_point() {
    atom="$1"
    case "$atom" in
        accept)   echo 0 ;;  # e1
        alphabet) echo 1 ;;  # e2
        left)     echo 2 ;;  # e3
        right)    echo 3 ;;  # e4
        delta)    echo 4 ;;  # e5
        start)    echo 5 ;;  # e6
        state)    echo 6 ;;  # e7
        reject)   echo -1 ;; # e0 (identity, implicit)
        *)        echo -1 ;; # unknown
    esac
}

# Point index to octonion unit name
point_to_unit() {
    case "$1" in
        0) echo "e1" ;;
        1) echo "e2" ;;
        2) echo "e3" ;;
        3) echo "e4" ;;
        4) echo "e5" ;;
        5) echo "e6" ;;
        6) echo "e7" ;;
        *) echo "e0" ;;
    esac
}

# Check if a set of points forms a closed subset under Fano lines
# A subset is closed if: for any line that contains 2 points from the set,
# the 3rd point must also be in the set (or the line is not touched)
check_fano_closure() {
    points="$1"  # space-separated point indices

    # Convert to set for membership testing
    point_set=" $points "

    # Check each Fano line (use process substitution to avoid subshell)
    while IFS= read -r line; do
        [ -z "$line" ] && continue

        p0=$(echo "$line" | cut -d, -f1)
        p1=$(echo "$line" | cut -d, -f2)
        p2=$(echo "$line" | cut -d, -f3)

        # Count how many points from this line are in our set
        count=0
        case "$point_set" in
            *" $p0 "*) count=$((count + 1)) ;;
        esac
        case "$point_set" in
            *" $p1 "*) count=$((count + 1)) ;;
        esac
        case "$point_set" in
            *" $p2 "*) count=$((count + 1)) ;;
        esac

        # If we have exactly 2 points from a line, closure is violated
        # (Having 0, 1, or 3 points is ok - only 2 violates closure)
        # Reason: if we have points A and B on a line, their "product" C must also be present
        if [ "$count" -eq 2 ]; then
            echo "VIOLATED: Line [$p0,$p1,$p2] has $count/3 points in set" >&2
            echo "          Points ($(point_to_unit "$p0"),$(point_to_unit "$p1"),$(point_to_unit "$p2")): exactly 2/3 present (missing closure)" >&2
            return 1
        fi
    done <<EOF
$FANO_LINES
EOF

    return 0
}

# Main validation logic
main() {
    # Extract atoms from CANB file
    atoms=$(extract_atoms)

    if [ -z "$atoms" ]; then
        echo "INVALID: No atoms found in CANB file" >&2
        exit 1
    fi

    # Map atoms to Fano points
    points=""
    residue=""

    for atom in $atoms; do
        pt=$(atom_to_point "$atom")
        if [ "$pt" -ge 0 ]; then
            points="$points $pt"
            unit=$(point_to_unit "$pt")
            residue="$residue $unit"
        fi
    done

    # Remove duplicates and sort
    points=$(echo "$points" | tr ' ' '\n' | grep -v '^$' | sort -u | tr '\n' ' ')
    residue=$(echo "$residue" | tr ' ' '\n' | grep -v '^$' | sort -u | tr '\n' ',' | sed 's/,$//')

    # Check if point set is closed under Fano lines
    if check_fano_closure "$points"; then
        echo "VALID: {$residue}"
        exit 0
    else
        echo "INVALID: Atom set violates Fano closure"
        echo "         Atoms: $atoms"
        echo "         Points: $points"
        echo "         Residue: {$residue}"
        exit 1
    fi
}

main
