# Building CANB Programs

## Introduction

This guide teaches you how to write sophisticated CANB programs, from simple graphs to complex relational structures.

## CANASM Syntax Reference

### Comments

```canasm
// Single-line comment (C++ style)
# Shell-style comment (also supported)
```

### Instructions

#### PUSH ATOM:name

Push an atom onto the stack.

**Syntax**: `PUSH ATOM:name`

**Stack effect**: `[] → [name]`

**Example**:
```canasm
PUSH ATOM:hello
// Stack: [hello]
```

#### EDGE_ADD

Pop two atoms from stack and create an edge.

**Syntax**: `EDGE_ADD`

**Stack effect**: `[src, dst] → []`

**Edge created**: `src → dst`

**Example**:
```canasm
PUSH ATOM:start
PUSH ATOM:end
EDGE_ADD
// Creates edge: start → end
// Stack: []
```

#### PROJ_FANO

Project top stack atom to Fano plane.

**Syntax**: `PROJ_FANO`

**Stack effect**: `[atom] → [atom]` (no change)

**Side effect**: Outputs projection event

**Example**:
```canasm
PUSH ATOM:accept
PROJ_FANO
// Projects accept (e₁) to Fano coordinates
```

#### HALT

Stop program execution.

**Syntax**: `HALT`

**Stack effect**: (terminates execution)

**Example**:
```canasm
PUSH ATOM:done
HALT
// Execution stops here
```

## Pattern Library

### Pattern 1: Single Edge

**Use case**: Create a simple directed edge

```canasm
// Edge: A → B
PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD
HALT
```

**Graph**:
```
A → B
```

### Pattern 2: Chain

**Use case**: Linear sequence of states

```canasm
// Chain: start → s1 → s2 → end

PUSH ATOM:start
PUSH ATOM:s1
EDGE_ADD

PUSH ATOM:s1
PUSH ATOM:s2
EDGE_ADD

PUSH ATOM:s2
PUSH ATOM:end
EDGE_ADD

HALT
```

**Graph**:
```
start → s1 → s2 → end
```

### Pattern 3: Star

**Use case**: Central node with multiple connections

```canasm
// Star: center connected to A, B, C

PUSH ATOM:center
PUSH ATOM:A
EDGE_ADD

PUSH ATOM:center
PUSH ATOM:B
EDGE_ADD

PUSH ATOM:center
PUSH ATOM:C
EDGE_ADD

HALT
```

**Graph**:
```
     A
     ↑
     |
center → B
     |
     ↓
     C
```

### Pattern 4: Cycle

**Use case**: Circular dependency or loop

```canasm
// Cycle: A → B → C → A

PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD

PUSH ATOM:B
PUSH ATOM:C
EDGE_ADD

PUSH ATOM:C
PUSH ATOM:A
EDGE_ADD

HALT
```

**Graph**:
```
A → B
↑   ↓
└─ C
```

### Pattern 5: Tree

**Use case**: Hierarchical structure

```canasm
// Binary tree
//     root
//    /    \
//   L      R
//  / \    / \
// L1 L2  R1 R2

// Level 1
PUSH ATOM:root
PUSH ATOM:L
EDGE_ADD

PUSH ATOM:root
PUSH ATOM:R
EDGE_ADD

// Level 2
PUSH ATOM:L
PUSH ATOM:L1
EDGE_ADD

PUSH ATOM:L
PUSH ATOM:L2
EDGE_ADD

PUSH ATOM:R
PUSH ATOM:R1
EDGE_ADD

PUSH ATOM:R
PUSH ATOM:R2
EDGE_ADD

HALT
```

### Pattern 6: DAG

**Use case**: Dependencies, dataflow

```canasm
// DAG: Multiple paths to same node
//   A
//  / \
// B   C
//  \ /
//   D

PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD

PUSH ATOM:A
PUSH ATOM:C
EDGE_ADD

PUSH ATOM:B
PUSH ATOM:D
EDGE_ADD

PUSH ATOM:C
PUSH ATOM:D
EDGE_ADD

HALT
```

## Working with Fano Atoms

### The Canonical 8-Tuple

For programs that will undergo Fano validation, use these atoms:

```canasm
// 8 canonical atoms
PUSH ATOM:accept    // e₁ (point 0)
PUSH ATOM:alphabet  // e₂ (point 1)
PUSH ATOM:left      // e₃ (point 2)
PUSH ATOM:right     // e₄ (point 3)
PUSH ATOM:delta     // e₅ (point 4)
PUSH ATOM:start     // e₆ (point 5)
PUSH ATOM:state     // e₇ (point 6)
PUSH ATOM:reject    // e₀ (identity)
```

### Fano Lines

Seven lines, each with exactly 3 points:

**Line 0**: accept, alphabet, left (e₁, e₂, e₃)
```canasm
PUSH ATOM:accept
PUSH ATOM:alphabet
PUSH ATOM:left
EDGE_ADD
EDGE_ADD
HALT
```

**Line 1**: accept, right, delta (e₁, e₄, e₅)
```canasm
PUSH ATOM:accept
PUSH ATOM:right
EDGE_ADD

PUSH ATOM:accept
PUSH ATOM:delta
EDGE_ADD

HALT
```

**Line 2**: accept, start, state (e₁, e₆, e₇)
```canasm
PUSH ATOM:accept
PUSH ATOM:start
EDGE_ADD

PUSH ATOM:accept
PUSH ATOM:state
EDGE_ADD

HALT
```

**Line 3**: alphabet, right, state (e₂, e₄, e₇)
**Line 4**: alphabet, delta, start (e₂, e₅, e₆)
**Line 5**: left, right, start (e₃, e₄, e₆)
**Line 6**: left, delta, state (e₃, e₅, e₇)

### Valid Atom Subsets

**Single point** (trivially closed):
```canasm
PUSH ATOM:accept
HALT
// VALID: {e1}
```

**Complete line** (closed):
```canasm
PUSH ATOM:accept
PUSH ATOM:alphabet
PUSH ATOM:left
EDGE_ADD
EDGE_ADD
HALT
// VALID: {e1,e2,e3}
```

**Multiple complete lines** (closed):
```canasm
// Lines 0 and 1
PUSH ATOM:accept    // e1 (shared)
PUSH ATOM:alphabet  // e2 (line 0)
PUSH ATOM:left      // e3 (line 0)
PUSH ATOM:right     // e4 (line 1)
PUSH ATOM:delta     // e5 (line 1)
EDGE_ADD
EDGE_ADD
EDGE_ADD
EDGE_ADD
HALT
// VALID: {e1,e2,e3,e4,e5}
```

**All 7 points** (fully closed):
```canasm
PUSH ATOM:accept
PUSH ATOM:alphabet
PUSH ATOM:left
PUSH ATOM:right
PUSH ATOM:delta
PUSH ATOM:start
PUSH ATOM:state
EDGE_ADD
EDGE_ADD
EDGE_ADD
EDGE_ADD
EDGE_ADD
EDGE_ADD
HALT
// VALID: {e1,e2,e3,e4,e5,e6,e7}
```

### Invalid Atom Subsets

**Incomplete line** (2/3 points):
```canasm
PUSH ATOM:accept
PUSH ATOM:alphabet
// Missing: left (to complete line 0)
EDGE_ADD
HALT
// INVALID: Line [0,1,2] has 2/3 points
```

**Why invalid**: If you use accept (e₁) and alphabet (e₂), they lie on line 0 with left (e₃). The Fano closure rule requires left to also be present.

## Advanced Techniques

### Technique 1: Labeled Edges

CANASM doesn't directly support edge labels, but you can encode them as atoms:

```canasm
// Edge: src -[label]-> dst
// Represented as: src → label, label → dst

PUSH ATOM:src
PUSH ATOM:LABEL_transition
EDGE_ADD

PUSH ATOM:LABEL_transition
PUSH ATOM:dst
EDGE_ADD

HALT
```

**Graph**:
```
src → LABEL_transition → dst
```

**Interpretation**: Edge from src to dst with label "transition"

### Technique 2: Multi-Edges

Multiple edges between same nodes:

```canasm
// Two different edges: A → B

PUSH ATOM:A
PUSH ATOM:B_via_path1
EDGE_ADD

PUSH ATOM:B_via_path1
PUSH ATOM:B
EDGE_ADD

PUSH ATOM:A
PUSH ATOM:B_via_path2
EDGE_ADD

PUSH ATOM:B_via_path2
PUSH ATOM:B
EDGE_ADD

HALT
```

**Graph**:
```
   ┌─→ B_via_path1 ─→ B
A ─┤
   └─→ B_via_path2 ─→ B
```

### Technique 3: Weighted Edges

Encode weights as intermediate atoms:

```canasm
// Edge: A -[weight:5]-> B

PUSH ATOM:A
PUSH ATOM:WEIGHT_5
EDGE_ADD

PUSH ATOM:WEIGHT_5
PUSH ATOM:B
EDGE_ADD

HALT
```

**Interpretation**: Edge from A to B with weight 5

### Technique 4: Hypergraphs

Simulate hyperedges (edges connecting > 2 nodes):

```canasm
// Hyperedge: {A, B, C} → D
// Represented as: A→H, B→H, C→H, H→D (H = hyperedge node)

PUSH ATOM:A
PUSH ATOM:HYPER_1
EDGE_ADD

PUSH ATOM:B
PUSH ATOM:HYPER_1
EDGE_ADD

PUSH ATOM:C
PUSH ATOM:HYPER_1
EDGE_ADD

PUSH ATOM:HYPER_1
PUSH ATOM:D
EDGE_ADD

HALT
```

**Graph**:
```
A ─┐
B ─┼→ HYPER_1 → D
C ─┘
```

## Example Programs

### Example 1: State Machine

```canasm
// Simple state machine for a door
//
// States: closed, open, locked
// Transitions:
//   closed → open (open door)
//   open → closed (close door)
//   closed → locked (lock door)
//   locked → closed (unlock door)

// Transitions
PUSH ATOM:closed
PUSH ATOM:open
EDGE_ADD

PUSH ATOM:open
PUSH ATOM:closed
EDGE_ADD

PUSH ATOM:closed
PUSH ATOM:locked
EDGE_ADD

PUSH ATOM:locked
PUSH ATOM:closed
EDGE_ADD

HALT
```

### Example 2: Dataflow Graph

```canasm
// Data processing pipeline
//
// input → parse → validate → process → output
//            ↓        ↓
//          error    error

PUSH ATOM:input
PUSH ATOM:parse
EDGE_ADD

PUSH ATOM:parse
PUSH ATOM:validate
EDGE_ADD

PUSH ATOM:parse
PUSH ATOM:error
EDGE_ADD

PUSH ATOM:validate
PUSH ATOM:process
EDGE_ADD

PUSH ATOM:validate
PUSH ATOM:error
EDGE_ADD

PUSH ATOM:process
PUSH ATOM:output
EDGE_ADD

HALT
```

### Example 3: Automaton (Fano-Valid)

```canasm
// Simple automaton using Fano atoms
//
// States: start, state, accept, reject
// Uses canonical atoms for Fano validation

// Transitions
PUSH ATOM:start
PUSH ATOM:state
EDGE_ADD

PUSH ATOM:state
PUSH ATOM:accept
EDGE_ADD

PUSH ATOM:state
PUSH ATOM:reject
EDGE_ADD

HALT

// Validation: {start, state, accept, reject}
// = {e₆, e₇, e₁, e₀}
// Points {1,6,7} do not form complete lines → depends on reject (e₀)
// If excluding reject (identity): {e₁,e₆,e₇} = Line 2 → VALID
```

### Example 4: Full Fano Algebra

```canasm
// Complete octonionic algebra (all 7 imaginary units)
// This represents the full Fano plane structure

PUSH ATOM:accept    // e₁
PUSH ATOM:alphabet  // e₂
PUSH ATOM:left      // e₃
PUSH ATOM:right     // e₄
PUSH ATOM:delta     // e₅
PUSH ATOM:start     // e₆
PUSH ATOM:state     // e₇

// Create edges representing multiplication table
// (Implementation would connect according to Fano lines)

EDGE_ADD
EDGE_ADD
EDGE_ADD
EDGE_ADD
EDGE_ADD
EDGE_ADD

HALT

// Validation: All 7 imaginary units → VALID: {e1,e2,e3,e4,e5,e6,e7}
```

## Optimization Techniques

### Atom Reuse

**Inefficient** (duplicate atoms):
```canasm
PUSH ATOM:node_A
PUSH ATOM:node_B
EDGE_ADD

PUSH ATOM:node_A  // Duplicate
PUSH ATOM:node_C
EDGE_ADD

HALT
```

**Efficient** (reuse atoms):
Atoms are automatically interned (deduplicated) by the assembler. Both approaches produce the same bytecode, but for clarity:

```canasm
// Clear structure
PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD

PUSH ATOM:A
PUSH ATOM:C
EDGE_ADD

HALT
```

### Minimize Stack Depth

**Deep stack** (5 atoms):
```canasm
PUSH ATOM:A
PUSH ATOM:B
PUSH ATOM:C
PUSH ATOM:D
PUSH ATOM:E
// Stack depth: 5
EDGE_ADD  // D→E
EDGE_ADD  // C→(D→E result)
EDGE_ADD  // B→...
EDGE_ADD  // A→...
HALT
```

**Shallow stack** (2 atoms):
```canasm
PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD

PUSH ATOM:C
PUSH ATOM:D
EDGE_ADD

PUSH ATOM:E
PUSH ATOM:F
EDGE_ADD

HALT
```

Current VM stack limit: 256 atoms

## Debugging

### Technique 1: Incremental Assembly

Build program incrementally, testing after each addition:

```bash
# Test 1: Single edge
cat > test1.canasm << 'EOF'
PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD
HALT
EOF

./bin/canasm0 test1.canasm -o test1.canb
./bin/canasm0-disasm test1.canb  # Verify

# Test 2: Add more edges
cat > test2.canasm << 'EOF'
PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD

PUSH ATOM:B
PUSH ATOM:C
EDGE_ADD

HALT
EOF

# Continue testing...
```

### Technique 2: Disassembly Inspection

```bash
# Assemble
./bin/canasm0 program.canasm -o program.canb

# Inspect disassembly
./bin/canasm0-disasm program.canb > program.dis

# Check atom table
grep -A 10 "@atoms" program.dis

# Check code
grep -A 20 "@code" program.dis
```

### Technique 3: Validation

```bash
# Check Fano validity
./integration/validators/canb-fano-validator.sh program.canb

# If invalid, check output
# Shows which Fano line is incomplete
```

### Technique 4: Hex Dump

```bash
# View raw bytecode
hexdump -C program.canb

# Check magic (should be "CANB")
hexdump -C program.canb | head -1
# 00000000  43 41 4e 42  01 ...

# Verify version (should be 0x01)
hexdump -C program.canb | head -1
# 00000000  43 41 4e 42  01 ...
#                        ^^
```

## Best Practices

### 1. Use Meaningful Atom Names

**Good**:
```canasm
PUSH ATOM:initial_state
PUSH ATOM:processing
PUSH ATOM:completed
```

**Bad**:
```canasm
PUSH ATOM:s1
PUSH ATOM:s2
PUSH ATOM:s3
```

### 2. Add Comments

```canasm
// State machine for user authentication
//
// States:
//   - logged_out: Initial state
//   - logged_in: After successful auth
//   - locked: After 3 failed attempts

PUSH ATOM:logged_out
PUSH ATOM:logged_in
EDGE_ADD

// ... etc
```

### 3. Validate Early

```bash
# Assemble
./bin/canasm0 program.canasm -o program.canb

# Validate immediately
./integration/validators/canb-fano-validator.sh program.canb

# Only proceed if valid
if [ $? -eq 0 ]; then
  echo "Program is valid"
else
  echo "Fix Fano closure violations"
  exit 1
fi
```

### 4. Test Round-Trip

```bash
# Assemble
./bin/canasm0 input.canasm -o a.canb

# Disassemble
./bin/canasm0-disasm a.canb > output.canasm

# Reassemble
./bin/canasm0 output.canasm -o b.canb

# Compare
cmp a.canb b.canb && echo "DETERMINISTIC" || echo "ERROR"
```

### 5. Use Version Control

```bash
# Track source files
git add program.canasm
git commit -m "Add initial state machine"

# Don't track binary files (add to .gitignore)
echo "*.canb" >> .gitignore
```

## Common Patterns Reference

| Pattern | Use Case | Example Atoms |
|---------|----------|---------------|
| Single Edge | Simple relation | `A → B` |
| Chain | Sequence | `start → s1 → s2 → end` |
| Star | Hub-and-spoke | `center → {A,B,C}` |
| Cycle | Loop | `A → B → C → A` |
| Tree | Hierarchy | `root → {L,R}` |
| DAG | Dependencies | `{A,B} → C` |
| State Machine | Transitions | `states → states` |
| Dataflow | Pipeline | `input → process → output` |

## Next Steps

- **[Validation Guide](validation-guide.md)** - Deep dive into Fano-gate
- **[Genesis Guide](genesis-guide.md)** - Master fingerprinting
- **[API Reference](../reference/canasm-api.md)** - Complete instruction set

---

**See examples**: `integration/examples/` (coming soon)
