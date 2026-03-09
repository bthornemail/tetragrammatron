# Fano Plane Reference

## Overview

The **Fano plane** is the smallest finite projective plane, consisting of 7 points and 7 lines, with exactly 3 points on each line and exactly 3 lines through each point.

In the Tetragrammatron Project, the Fano plane serves as the **multiplication diagram** for octonions, providing the mathematical foundation for validation and projection.

## Structure

### Points

The 7 points represent the 7 imaginary octonion units:

| Index | Symbol | Atom Name | Description |
|-------|--------|-----------|-------------|
| 0 | e₁ | `accept` | First imaginary unit |
| 1 | e₂ | `alphabet` | Second imaginary unit |
| 2 | e₃ | `left` | Third imaginary unit |
| 3 | e₄ | `right` | Fourth imaginary unit |
| 4 | e₅ | `delta` | Fifth imaginary unit |
| 5 | e₆ | `start` | Sixth imaginary unit |
| 6 | e₇ | `state` | Seventh imaginary unit |

**Identity**: e₀ (`reject`) is the real unit (identity element)

### Lines

The 7 lines represent multiplication triads:

| Line | Points | Atoms | Octonion Units |
|------|--------|-------|----------------|
| 0 | {0, 1, 2} | {accept, alphabet, left} | {e₁, e₂, e₃} |
| 1 | {0, 3, 4} | {accept, right, delta} | {e₁, e₄, e₅} |
| 2 | {0, 5, 6} | {accept, start, state} | {e₁, e₆, e₇} |
| 3 | {1, 3, 6} | {alphabet, right, state} | {e₂, e₄, e₇} |
| 4 | {1, 4, 5} | {alphabet, delta, start} | {e₂, e₅, e₆} |
| 5 | {2, 3, 5} | {left, right, start} | {e₃, e₄, e₆} |
| 6 | {2, 4, 6} | {left, delta, state} | {e₃, e₅, e₇} |

### Incidence Properties

**Every point lies on exactly 3 lines**:
- Point 0 (e₁/accept): Lines {0, 1, 2}
- Point 1 (e₂/alphabet): Lines {0, 3, 4}
- Point 2 (e₃/left): Lines {0, 5, 6}
- Point 3 (e₄/right): Lines {1, 3, 5}
- Point 4 (e₅/delta): Lines {1, 4, 6}
- Point 5 (e₆/start): Lines {2, 4, 5}
- Point 6 (e₇/state): Lines {2, 3, 6}

**Every line contains exactly 3 points**: See table above

**Any two points determine a unique line**:
- Points 0,1 → Line 0
- Points 0,3 → Line 1
- Points 1,4 → Line 4
- etc.

**Any two lines intersect at exactly one point**:
- Lines 0,1 → Point 0
- Lines 0,3 → Point 1
- Lines 2,4 → Point 5
- etc.

## Octonion Multiplication

### Multiplication Table

The Fano plane encodes the multiplication rules for octonions. For imaginary units:

**Rule**: If {i, j, k} form a Fano line (in order), then:
- i × j = k
- j × k = i
- k × i = j

**Orientation**: Each line has a direction (indicated by arrows in traditional diagrams).

### Example: Line 0

Line 0 = {e₁, e₂, e₃}

**Multiplications**:
- e₁ × e₂ = e₃
- e₂ × e₃ = e₁
- e₃ × e₁ = e₂

**Anti-commutativity**:
- e₂ × e₁ = -e₃
- e₃ × e₂ = -e₁
- e₁ × e₃ = -e₂

### Full Multiplication Table

```
     | e₀  e₁  e₂  e₃  e₄  e₅  e₆  e₇
-----+--------------------------------
e₀   | e₀  e₁  e₂  e₃  e₄  e₅  e₆  e₇
e₁   | e₁ -e₀  e₃ -e₂  e₅ -e₄ -e₇  e₆
e₂   | e₂ -e₃ -e₀  e₁  e₆  e₇ -e₄ -e₅
e₃   | e₃  e₂ -e₁ -e₀  e₇ -e₆  e₅ -e₄
e₄   | e₄ -e₅ -e₆ -e₇ -e₀  e₁  e₂  e₃
e₅   | e₅  e₄ -e₇  e₆ -e₁ -e₀ -e₃  e₂
e₆   | e₆  e₇  e₄ -e₅ -e₂  e₃ -e₀ -e₁
e₇   | e₇ -e₆  e₅  e₄ -e₃ -e₂  e₁ -e₀
```

**Properties**:
- e₀ is the identity: e₀ × eᵢ = eᵢ × e₀ = eᵢ
- Self-multiplication: eᵢ × eᵢ = -e₀ (for i ≠ 0)
- Non-commutative: eᵢ × eⱼ ≠ eⱼ × eᵢ (in general)
- Non-associative: (eᵢ × eⱼ) × eₖ ≠ eᵢ × (eⱼ × eₖ) (in general)

## Closure Properties

### Definition

A subset S ⊆ {e₁, e₂, e₃, e₄, e₅, e₆, e₇} is **Fano-closed** if:

∀ a,b ∈ S: If line(a,b) = {a,b,c}, then c ∈ S

**Interpretation**: If we have two imaginary units that lie on the same Fano line, their "product" (the third unit on that line) must also be in the set.

### Valid Closed Subsets

**Trivial closures**:
- {} (empty set)
- {eᵢ} for any single i ∈ {1,2,3,4,5,6,7}

**Complete lines** (7 total):
- {e₁, e₂, e₃} (line 0)
- {e₁, e₄, e₅} (line 1)
- {e₁, e₆, e₇} (line 2)
- {e₂, e₄, e₇} (line 3)
- {e₂, e₅, e₆} (line 4)
- {e₃, e₄, e₆} (line 5)
- {e₃, e₅, e₇} (line 6)

**Union of disjoint lines**:
- {e₁, e₂, e₃, e₄, e₇} = lines {0, 3}
- {e₁, e₄, e₅, e₆, e₇} = lines {1, 2}
- Many other combinations...

**Full set**:
- {e₁, e₂, e₃, e₄, e₅, e₆, e₇} (all 7 imaginary units)

### Invalid Subsets

**Incomplete lines** (2/3 of a line):
- {e₁, e₂} (missing e₃ from line 0)
- {e₁, e₄} (missing e₅ from line 1)
- {e₂, e₄} (missing e₇ from line 3)
- etc.

**Why invalid**: If we have e₁ and e₂, we can compute e₁ × e₂ = e₃. Since e₃ is not in the set, closure is violated.

## Geometric Representations

### 2D Projection (Traditional)

```
       e₁ (0)
        *
       /|\
      / | \
     /  |  \
    / L₀|   \
   /    |    \
  /     |     \
e₂ ---- * ---- e₃
(1)    center  (2)

     +

   e₅       e₆
   (4)     (5)
    *-------*
     \     /
      \   /
       \ /
        *
       e₄
       (3)

     +

        *
       e₇
       (6)
```

**Note**: This is a schematic - the actual Fano plane cannot be embedded in 2D Euclidean space without edge crossings.

### 3D Projection (Merkaba)

The Merkaba (dual tetrahedron / star tetrahedron) is a 3D embedding:

**Tetrahedron 1 (upward)**:
- e₁ at ( 1,  1,  1)
- e₂ at (-1, -1,  1)
- e₃ at (-1,  1, -1)
- e₄ at ( 1, -1, -1)

**Tetrahedron 2 (downward)**:
- e₅ at (-1,  1,  1)
- e₆ at ( 1, -1,  1)
- e₇ at ( 1,  1, -1)

**Fano lines as triangles**:
- Each of the 7 lines forms a triangle in 3D space
- Lines are color-coded in visualization

**Visualization**: See `tetragrammatron-os/web/demo/glb_demo.html`

## Applications in Tetragrammatron

### 1. Fano-Gate Validation

**Purpose**: Ensure CANB programs satisfy closure

**Implementation**: `integration/validators/canb-fano-validator.sh`

**Algorithm**:
```
For each Fano line L = {a, b, c}:
  count = |L ∩ program_atoms|
  if count == 2:
    REJECT "Incomplete line (closure violated)"
ACCEPT
```

**Example**:
```canasm
// Program atoms: {accept, alphabet, left}
// = {e₁, e₂, e₃}
// = Line 0 (complete)
// → VALID
```

### 2. Graph Projection

**SVG Projection** (2D):
- Maps each atom to a 2D coordinate
- Draws edges as lines
- Colors based on Fano line membership

**GLB Projection** (3D):
- Maps each atom to Merkaba vertex
- Draws edges as line segments
- Exports to glTF binary format

### 3. Type System (Future)

**Idea**: Use Fano closure as type constraint

**Example**:
```
Function signature: (e₁, e₂) → e₃

Type rule: If function takes two atoms on a Fano line,
           it must return the third atom on that line.

Valid call:
  f(accept, alphabet) → left  ✓

Invalid call:
  f(accept, alphabet) → right  ✗ (right not on line 0)
```

## Mathematical Properties

### Projective Plane Axioms

The Fano plane satisfies the axioms of a finite projective plane:

1. **Any two distinct points lie on exactly one line**
2. **Any two distinct lines meet at exactly one point**
3. **There exist four points, no three of which are collinear**

### Symmetry Group

The Fano plane has a symmetry group of order 168 (PSL(3,2) or PSL(2,7)).

**Automorphisms**: 168 symmetries preserving incidence structure

**Implications**:
- All points are "equivalent" (up to symmetry)
- All lines are "equivalent" (up to symmetry)
- No distinguished point or line

### Connection to Other Structures

**Steiner system**: S(2,3,7) - every pair of points lies in exactly one block of size 3

**Coding theory**: Hamming(7,4) code - 7-bit codewords with 4 data bits

**Group theory**: Simple group of order 168 (smallest non-abelian simple group)

## Computational Complexity

### Operations

| Operation | Complexity | Description |
|-----------|------------|-------------|
| Point on line? | O(1) | Check if point index in line's set |
| Line through points? | O(7) | Search all 7 lines |
| Lines through point? | O(1) | Lookup (3 lines per point) |
| Closure check | O(7 × n) | Check all lines for n-atom set |

**Optimization**: Precompute lookup tables for:
- `line_contains[line][point] → bool`
- `point_lines[point] → [line_0, line_1, line_2]`
- `points_to_line[point_a][point_b] → line`

### Fano-Gate Validation

**Worst case**: O(7 × 7) = O(49) for full 7-atom program

**Average case**: O(7 × n) for n atoms (n typically < 5)

**Best case**: O(1) for 0 or 1 atoms (trivially valid)

## Code Examples

### JavaScript (Fano Line Data)

```javascript
// Fano plane structure
export const FANO_LINES = [
  [0, 1, 2],  // Line 0: e₁, e₂, e₃
  [0, 3, 4],  // Line 1: e₁, e₄, e₅
  [0, 5, 6],  // Line 2: e₁, e₆, e₇
  [1, 3, 6],  // Line 3: e₂, e₄, e₇
  [1, 4, 5],  // Line 4: e₂, e₅, e₆
  [2, 3, 5],  // Line 5: e₃, e₄, e₆
  [2, 4, 6],  // Line 6: e₃, e₅, e₇
];

// Check if point is on line
function pointOnLine(point, line) {
  return FANO_LINES[line].includes(point);
}

// Find line containing two points
function findLine(p1, p2) {
  for (let i = 0; i < 7; i++) {
    const line = FANO_LINES[i];
    if (line.includes(p1) && line.includes(p2)) {
      return i;
    }
  }
  return null;
}
```

### Shell (Closure Validation)

```bash
# Check if atom set is Fano-closed
check_fano_closure() {
  points="$1"  # Space-separated point indices

  for line in "0,1,2" "0,3,4" "0,5,6" "1,3,6" "1,4,5" "2,3,5" "2,4,6"; do
    p0=$(echo "$line" | cut -d, -f1)
    p1=$(echo "$line" | cut -d, -f2)
    p2=$(echo "$line" | cut -d, -f3)

    # Count how many points from this line are in our set
    count=0
    echo "$points" | grep -q " $p0 " && count=$((count + 1))
    echo "$points" | grep -q " $p1 " && count=$((count + 1))
    echo "$points" | grep -q " $p2 " && count=$((count + 1))

    # Violation: exactly 2/3 points
    if [ "$count" -eq 2 ]; then
      echo "INVALID: Line [$line] incomplete"
      return 1
    fi
  done

  echo "VALID"
  return 0
}
```

## Visualization Tools

### Available Visualizations

1. **SVG (2D)**: `tetragrammatron-os/web/demo/index.html`
   - Interactive Fano plane diagram
   - Shows all 7 points and 7 lines
   - Hoverable/clickable elements

2. **GLB (3D)**: `tetragrammatron-os/web/demo/glb_demo.html`
   - Merkaba (dual tetrahedron) representation
   - Exportable to glTF format
   - Importable into Blender, Three.js, etc.

3. **ASCII Art** (Terminal):
```
    e1
   / |\
  /  | \
 /   |  \
e2---+---e3
     |
    ...
```

## References

### Mathematical

- **Finite projective planes**: Dembowski, P. (1968). *Finite Geometries*
- **Octonions**: Baez, J. (2001). *The Octonions*. Bull. AMS.
- **Steiner systems**: Beth, T., Jungnickel, D., Lenz, H. (1999). *Design Theory*

### Tetragrammatron-Specific

- [Fano-Gate Validator](../../integration/validators/canb-fano-validator.sh)
- [Merkaba Projection](../../tetragrammatron-os/web/demo/fano_merkaba.js)
- [Integration Guide](../../integration/README.md)

## Summary

The Fano plane provides:

- **7 points** (imaginary octonion units)
- **7 lines** (multiplication triads)
- **Closure property** (validation constraint)
- **Geometric representation** (2D/3D visualization)

In Tetragrammatron, the Fano plane serves as:

1. **Validation gate** - Reject non-closed programs
2. **Type system** - Enforce algebraic constraints
3. **Visualization** - Project to SVG/GLB
4. **Mathematical foundation** - Octonion algebra

---

**See also**:
- [Validation Guide](../guides/validation-guide.md)
- [CANB Specification](canb-specification.md)
- [Architecture Overview](../architecture/overview.md)
