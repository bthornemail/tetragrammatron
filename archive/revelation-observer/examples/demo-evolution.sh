#!/bin/sh
# demo-evolution.sh - Track structural evolution across versions
# Shows how code structure changes (or doesn't) across modifications

set -eu

DIFFUSE="../bin/genesis-diffuse.sh"

echo "=== Revelation Observer: Structural Evolution ==="
echo ""
echo "Tracking how structure evolves across file versions..."
echo ""

# Create three versions of a file
mkdir -p "$HOME/tmp/evolution"

# Version 1: Simple function
cat > "$HOME/tmp/evolution/v1.c" << 'EOF'
int add(int a, int b) {
    return a + b;
}
EOF

# Version 2: Add comments (structure unchanged)
cat > "$HOME/tmp/evolution/v2.c" << 'EOF'
// Addition function
int add(int a, int b) {
    return a + b;  // Simple addition
}
EOF

# Version 3: Change implementation (structure changes)
cat > "$HOME/tmp/evolution/v3.c" << 'EOF'
int add(int a, int b) {
    int result = a;
    result = result + b;
    return result;
}
EOF

echo "Version 1: Simple function"
echo "---"
cat "$HOME/tmp/evolution/v1.c"
echo ""
basis_v1=$("$DIFFUSE" "$HOME/tmp/evolution/v1.c")
echo "Basis: $basis_v1"
echo ""

echo "Version 2: Added comments"
echo "---"
cat "$HOME/tmp/evolution/v2.c"
echo ""
basis_v2=$("$DIFFUSE" "$HOME/tmp/evolution/v2.c")
echo "Basis: $basis_v2"
echo ""

echo "Version 3: Changed implementation"
echo "---"
cat "$HOME/tmp/evolution/v3.c"
echo ""
basis_v3=$("$DIFFUSE" "$HOME/tmp/evolution/v3.c")
echo "Basis: $basis_v3"
echo ""

echo "=== Evolution Analysis ==="
echo ""

if [ "$basis_v1" = "$basis_v2" ]; then
    echo "v1 → v2: STRUCTURALLY IDENTICAL (comments don't change structure)"
else
    echo "v1 → v2: STRUCTURALLY DIFFERENT"
fi

if [ "$basis_v2" = "$basis_v3" ]; then
    echo "v2 → v3: STRUCTURALLY IDENTICAL"
else
    echo "v2 → v3: STRUCTURALLY DIFFERENT (implementation changed)"
fi

if [ "$basis_v1" = "$basis_v3" ]; then
    echo "v1 → v3: STRUCTURALLY IDENTICAL"
else
    echo "v1 → v3: STRUCTURALLY DIFFERENT"
fi

# Cleanup
rm -rf "$HOME/tmp/evolution"

echo ""
echo "=== Key Insight ==="
echo "Comments and formatting don't affect structural basis."
echo "Only syntactic structure matters (via regex diffusion)."
echo ""
echo "=== Demo Complete ==="
