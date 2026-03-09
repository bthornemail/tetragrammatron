# Integration: Genesis Protocol + Tetragrammatron-OS

This directory contains integration artifacts combining Genesis Protocol with Tetragrammatron-OS.

## Overview

The integration enables:
- **Provenance-verified CANB programs** - Every `.canb` file can be wrapped with Genesis structural fingerprints
- **Fano-gate validation** - Reject programs that violate octonionic closure constraints
- **Distributed sync** - Exchange structural atoms between nodes without Git or central authority
- **Mathematically-validated execution** - Only accept programs that satisfy Fano plane closure

## Files

- `.genesis` - Functor table (UU/KU/KK routing rules)
- `.genesisignore` - Exclusion patterns for fingerprinting
- `.genesisinclude` - Inclusion patterns for fingerprinting
- `tetragrammatron-atoms.jsonl` - 16-atom structural database with HW/V8 fingerprints
- `validators/` - Fano-gate and other validators
- `examples/` - Integration examples and demos

## Fano-Gate Validator

The Fano-gate validator ensures CANB programs satisfy **octonionic closure** - a mathematical constraint based on the Fano plane (7 points, 7 lines).

### Background

The Fano plane is the multiplication table of octonions (7 imaginary units + 1 real). A CANB program's atom set must form a **closed sub-algebra** under this structure.

**Closure rule**: If atoms A and B are present, and they lie on the same Fano line with atom C, then C must also be present.

**Canonical 8-tuple mapping**:
- `accept` → e₁ (point 0)
- `alphabet` → e₂ (point 1)
- `left` → e₃ (point 2)
- `right` → e₄ (point 3)
- `delta` → e₅ (point 4)
- `start` → e₆ (point 5)
- `state` → e₇ (point 6)
- `reject` → e₀ (identity, implicit)

**Fano lines** (each line must be complete or not touched):
```
Line 0: [accept, alphabet, left]     = {e₁, e₂, e₃}
Line 1: [accept, right, delta]       = {e₁, e₄, e₅}
Line 2: [accept, start, state]       = {e₁, e₆, e₇}
Line 3: [alphabet, right, state]     = {e₂, e₄, e₇}
Line 4: [alphabet, delta, start]     = {e₂, e₅, e₆}
Line 5: [left, right, start]         = {e₃, e₄, e₆}
Line 6: [left, delta, state]         = {e₃, e₅, e₇}
```

### Usage

```bash
cd integration/validators/

# Validate a CANB program
./canb-fano-validator.sh <program.canb>

# Examples
./canb-fano-validator.sh test-valid.canb     # VALID: {e1,e2,e3}
./canb-fano-validator.sh test-invalid.canb   # INVALID: Line [0,1,2] has 2/3 points
```

### Exit Codes

- `0` - Program is VALID (satisfies Fano closure)
- `1` - Program is INVALID (violates closure or other error)
- `2` - Usage error or missing dependencies

### Examples

**Valid program** (complete Fano line):
```canasm
// Atoms: {accept, alphabet, left} = Line 0 = {e₁, e₂, e₃}
PUSH ATOM:accept
PUSH ATOM:alphabet
EDGE_ADD
PUSH ATOM:left
EDGE_ADD
HALT
```

**Invalid program** (incomplete Fano line):
```canasm
// Atoms: {accept, alphabet} = 2/3 of Line 0 (missing left)
PUSH ATOM:accept
PUSH ATOM:alphabet
EDGE_ADD
HALT
```

**Also valid** (disjoint points):
```canasm
// Atoms: {accept} = 1/7 points (no complete line touched)
PUSH ATOM:accept
HALT
```

**Also valid** (empty residue):
```canasm
// Atoms: {alpha, beta} = non-canonical atoms (not in 8-tuple)
PUSH ATOM:alpha
PUSH ATOM:beta
EDGE_ADD
HALT
```

### Implementation

The validator:
1. Extracts atoms from CANB using the disassembler
2. Maps canonical atom names to Fano point indices
3. Checks each Fano line for partial matches (exactly 2/3 points = violation)
4. Returns VALID with residue set or INVALID with violation reason

## Genesis Fingerprinting

Generate structural fingerprints for CANB programs:

```bash
cd ../genesis-protocol/

# Standard ASCII fingerprint
./genesis.sh translate ../tetragrammatron-os/bin/canasm0

# Octonionic symbols (e₀/Σ/·)
GENESIS_P0_SYMBOL=e0 GENESIS_P1_SYMBOL=Σ GENESIS_WS_SYMBOL=· \
  ./genesis-enhanced.sh translate ../tetragrammatron-os/bin/canasm0

# Greek symbols (α/Ω/∘)
GENESIS_P0_SYMBOL=α GENESIS_P1_SYMBOL=Ω GENESIS_WS_SYMBOL=∘ \
  ./genesis-enhanced.sh translate ../tetragrammatron-os/bin/canasm0
```

Output includes:
- **HW** - 8-byte hardware fingerprint (platform constraints)
- **V8** - 8-integer topology vector (structural shape)
- **FORM** - P₀/P₁ skeleton (alternation pattern)

## Atoms Database

The `tetragrammatron-atoms.jsonl` file contains structural fingerprints for all tracked files:

```bash
# View atoms database
cat tetragrammatron-atoms.jsonl | head -3

# Extract specific fields
grep 'canasm0.c' tetragrammatron-atoms.jsonl

# Count atoms
wc -l tetragrammatron-atoms.jsonl   # 16 atoms
```

Each atom entry contains:
- `t` - Timestamp
- `path` - File path
- `id` - Normalized identifier
- `stat` - POSIX metadata (dev, ino, mode, size, times)
- `hw` - 8-byte hardware fingerprint
- `v` - 8-integer topology vector

## Future Integration Work

- **Peer sync demo** - Two-node structural reconciliation without Git
- **GLB with provenance** - 3D Merkaba export with Genesis metadata
- **Remote program validation** - Accept CANB from peers only if Fano-valid
- **Provenance chains** - Link CANB execution history via Genesis atoms

