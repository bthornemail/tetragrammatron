#!/bin/sh
# demo-comparison.sh - Demonstrate structural basis comparison
# Shows how different files have different structural fingerprints

set -eu

DIFFUSE="../bin/genesis-diffuse.sh"

echo "=== Revelation Observer: Structural Basis Comparison ==="
echo ""

# Test files from tetragrammatron-os
FILES="
../../tetragrammatron-os/src/canasm/seed/canasm0.c
../../tetragrammatron-os/src/canasm/seed/canasm0_disasm.c
../../tetragrammatron-os/src/canb/wasm/canvm_wasm.c
"

echo "Computing structural basis for each file..."
echo ""

for file in $FILES; do
    if [ -f "$file" ]; then
        basename=$(basename "$file")
        basis=$("$DIFFUSE" "$file")
        echo "$basename"
        echo "  Basis: $basis"
        echo ""
    fi
done

echo "=== Comparison ==="
echo ""
echo "Different files → different basis hashes"
echo "This demonstrates structural uniqueness without semantic interpretation"
echo ""

# Compare two similar files
echo "=== Testing Structural Equivalence ==="
echo ""

# Create two structurally identical files with different content
mkdir -p "$HOME/tmp"

cat > "$HOME/tmp/file_a.txt" << 'EOF'
hello world
foo bar baz
EOF

cat > "$HOME/tmp/file_b.txt" << 'EOF'
goodbye moon
zip zap zoom
EOF

basis_a=$("$DIFFUSE" "$HOME/tmp/file_a.txt")
basis_b=$("$DIFFUSE" "$HOME/tmp/file_b.txt")

echo "File A: hello world / foo bar baz"
echo "  Basis: $basis_a"
echo ""
echo "File B: goodbye moon / zip zap zoom"
echo "  Basis: $basis_b"
echo ""

if [ "$basis_a" = "$basis_b" ]; then
    echo "Result: STRUCTURALLY IDENTICAL (same pattern)"
else
    echo "Result: STRUCTURALLY DIFFERENT"
fi

# Cleanup
rm -f "$HOME/tmp/file_a.txt" "$HOME/tmp/file_b.txt"

echo ""
echo "=== Demo Complete ==="
