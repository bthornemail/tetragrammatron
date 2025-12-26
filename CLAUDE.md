# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The **Tetragrammatron Project** combines two complementary systems:

1. **Tetragrammatron-OS** - A relational computational substrate (computation without numbers)
2. **Genesis Protocol** - A structural provenance system (coordination without authority)

Together they form a complete ecosystem for distributed, mathematically-verified, hardware-agnostic computation.

---

## Repository Structure

```
tetragrammatron/
├── tetragrammatron-os/      ← Core OS implementation
│   ├── src/                 ← CANASM assembler + CANB VM
│   ├── bin/                 ← Compiled binaries
│   ├── web/                 ← Browser demos (SVG, GLB)
│   ├── docs/                ← OS-specific documentation
│   └── Makefile
├── genesis-protocol/        ← Distributed repo system
│   ├── genesis.sh           ← Standard implementation
│   ├── genesis-enhanced.sh  ← With customizable symbols
│   └── docs/                ← Genesis-specific docs
├── integration/             ← Genesis + Tetragrammatron
│   ├── .genesis*            ← Control files
│   ├── tetragrammatron-atoms.jsonl  ← Atoms database
│   ├── validators/          ← Fano-gate, etc.
│   └── examples/            ← Integration demos
├── docs/                    ← Top-level documentation
│   ├── Tetragrammatron-OS.md  ← White paper
│   ├── CONTRIBUTING.md
│   └── guides/
├── tools/                   ← Utility scripts
└── dev/                     ← Development/experimental
    ├── archive/             ← Old versions
    ├── research/            ← Analysis documents
    └── experiments/         ← Prototypes
```

---

## Common Commands

### Building Tetragrammatron-OS

```bash
cd tetragrammatron-os/
make                    # Build canasm0 + canasm0-disasm

# Test assembler
./bin/canasm0 src/canasm/seed/canasm1.canasm -o ~/demo.canb

# Test disassembler
./bin/canasm0-disasm ~/demo.canb

# Verify determinism
./bin/canasm0 src/canasm/seed/canasm1.canasm -o ~/demo2.canb
cmp ~/demo.canb ~/demo2.canb  # Should be identical
```

### Using Genesis Protocol

```bash
cd genesis-protocol/

# Standard fingerprint
./genesis.sh translate ~/demo.canb

# With custom symbols (Octonionic mode)
GENESIS_P0_SYMBOL=e0 GENESIS_P1_SYMBOL=Σ GENESIS_WS_SYMBOL=· \
  ./genesis-enhanced.sh translate ~/demo.canb

# Generate atoms database
./genesis.sh probe ../tetragrammatron-os/ atoms.jsonl

# Show 8-dimensional topology
./genesis.sh mux8 ~/demo.canb
```

### Web Demos

```bash
cd tetragrammatron-os/web/demo/

# SVG Fano plane demo
python3 -m http.server 8000
# Open http://localhost:8000

# GLB Merkaba export demo
# Open http://localhost:8000/glb_demo.html
```

---

## Architecture (Tetragrammatron-OS)

### Three-Layer Architecture

1. **CANASM** (Assembler)
   - Input: `.canasm` text files with `@atoms` and `@code` sections
   - Output: `.canb` binary containers
   - Primitives: `ATOM`, `EDGE`, `PROJ_FANO`, `HALT`

2. **CANB** (Bytecode)
   - Magic: `CANB` (4 bytes)
   - Atom table: ULEB128-encoded symbols
   - Code: Platform-independent bytecode stream

3. **CANVM** (Execution)
   - Stack-based VM with graph/relation store
   - Operations are relation traversals, not numeric computations
   - Deterministic (same input → same output)

### The Canonical 8-Tuple

Normalized semantic atoms (aligned with octonion structure):
- `accept`, `alphabet`, `left`, `right`, `delta`, `start`, `state`, `reject`

This provides 7 fundamental relations + 1 identity (octonionic closure).

### Hardware and POSIX as Projections

- **Hardware (RFC-0016)**: Capabilities as atoms + edges (not numeric IDs)
- **POSIX (RFC-0017)**: Paths as traversals, FDs as ephemeral tokens

---

## Architecture (Genesis Protocol)

### Two-Primitive Projection Model (TPPM)

Everything reduces to two primitives:

- **P₀** (Semantic Mass): `[[:alnum:]]+` - alphanumeric runs
- **P₁** (Structure): `[^[:alnum:]]+` - non-alphanumeric runs

**Key principle**: Symbols don't matter, only shape (alternation pattern).

### Customizable Symbols ⭐

Use environment variables to customize P₀/P₁ representation:

```bash
# Default ASCII
FORM=AAAASSSAAAAASAAAASSSSSSSS_

# Greek Alpha/Omega
GENESIS_P0_SYMBOL=α GENESIS_P1_SYMBOL=Ω
FORM=ααααΩΩΩαααααΩααααΩΩΩΩΩΩΩΩ∘

# Octonionic
GENESIS_P0_SYMBOL=e0 GENESIS_P1_SYMBOL=Σ
FORM=e0e0e0e0ΣΣΣe0e0e0e0e0Σe0e0e0e0ΣΣΣΣΣΣΣΣ·
```

All structurally equivalent!

### Layer Model (0-7)

| Layer | Role | Tetragrammatron Analogy |
|-------|------|------------------------|
| −0 | Constraint (hardware) | Hardware-as-Ball |
| 0 | Existence (POSIX stat) | Inode atoms |
| 1 | Selection (include/ignore) | Query filters |
| 2 | Identity (content hash) | Blake3 atoms |
| 3 | Relation (structure) | Relation edges |
| 7 | Judgement (consensus) | Fano-gate |

---

## Integration Points

### 1. CANB Files with Genesis Provenance

Every `.canb` file can be wrapped with Genesis metadata:

```bash
cd integration/
../genesis-protocol/genesis.sh compile ../tetragrammatron-os/bin/canasm0 canasm0.genesis.org
```

Result: Immutable provenance chain (hardware fingerprint + content hash).

### 2. Fano-Gate Validation

Before accepting remote CANB programs:

```bash
cd integration/validators/
./canb-fano-validator.sh remote.canb
# Output: VALID (residue: {e1,e3,e5}) or INVALID (reason: not closed)
```

### 3. Distributed Sync

Two nodes exchange structural atoms:

```bash
# Node A
genesis.sh probe tetragrammatron-os/ atoms-A.jsonl

# Node B
genesis.sh probe tetragrammatron-os/ atoms-B.jsonl

# Reconcile (finds structural differences)
genesis.sh reconcile atoms-A.jsonl atoms-B.jsonl diff.jsonl
```

### 4. GLB Export with Provenance

```javascript
// In fano_merkaba.js
import { generateMerkabaGLB } from './fano_merkaba.js';

// Add Genesis metadata to GLB
const glb = generateMerkabaGLB();
glb.metadata = {
  genesis_hw: "6B:34:51:4B:4B:65:4D:4D",
  genesis_v8: [83,70,71,14,98,140,29,140],
  fano_residue: ["e1", "e3", "e5"]
};
```

---

## Development Principles

### Invariants to Preserve

1. **No numeric indices as canonical truth**
2. **Deterministic operations** (graph queries use stable ordering)
3. **Projection is non-semantic** (visualization doesn't mutate graph)
4. **Pure relations** (all semantics are atom edges)
5. **Symbols are views** (P₀/P₁ shape matters, not glyphs)

### What NOT to Do

- ❌ Treat numbers as canonical identity
- ❌ Assume clocks define correctness
- ❌ Encode meaning into hardware properties
- ❌ Add features without preserving invariants

---

## Key Files

### Tetragrammatron-OS

**Build**:
- `tetragrammatron-os/Makefile` - Main build system
- `tetragrammatron-os/src/canasm/seed/canasm0.c` - Seed assembler (6.8K)
- `tetragrammatron-os/src/canasm/seed/canasm0_disasm.c` - Disassembler (3.8K)

**Specs**:
- `tetragrammatron-os/src/canb/spec/CANISA_CANB_BRIDGE.md` - Macro expansion rules
- `tetragrammatron-os/docs/RFC-0016-Hardware-as-Relations.md`
- `tetragrammatron-os/docs/RFC-0017-POSIX-as-Projection.md`

**Web**:
- `tetragrammatron-os/web/demo/index.html` - SVG Fano demo
- `tetragrammatron-os/web/demo/glb_demo.html` - 3D Merkaba demo
- `tetragrammatron-os/web/demo/fano_merkaba.js` - GLB generator (6.3K)

### Genesis Protocol

**Core**:
- `genesis-protocol/genesis.sh` - Standard POSIX implementation (8.2K)
- `genesis-protocol/genesis-enhanced.sh` - With customizable symbols (NEW!)
- `genesis-protocol/GENESIS.org` - Canonical declaration

**Control**:
- `integration/.genesisignore` - Exclusion patterns
- `integration/.genesisinclude` - Inclusion patterns
- `integration/.genesis` - Functor table (UU/KU/KK routing)

**Database**:
- `integration/tetragrammatron-atoms.jsonl` - 16 atoms with fingerprints

---

## Testing

### CANB Round-trip

```bash
cd tetragrammatron-os/

# Assemble
./bin/canasm0 src/canasm/seed/canasm1.canasm -o test1.canb

# Disassemble
./bin/canasm0-disasm test1.canb > test.canasm

# Reassemble (requires matching assembler syntax)
# Note: Disassembler outputs canonical form, assembler expects minimal syntax
# This is intentional separation
```

### Genesis Determinism

```bash
cd integration/

# Same file, multiple fingerprints
../genesis-protocol/genesis.sh translate file.c > fp1.txt
../genesis-protocol/genesis.sh translate file.c > fp2.txt
diff fp1.txt fp2.txt  # Should be identical
```

### Symbol Equivalence

```bash
# Three different symbols, same structure
GENESIS_P0_SYMBOL=A ./genesis-enhanced.sh translate file.c > ascii.txt
GENESIS_P0_SYMBOL=α ./genesis-enhanced.sh translate file.c > greek.txt
GENESIS_P0_SYMBOL=e0 ./genesis-enhanced.sh translate file.c > octo.txt

# Extract run-length patterns (should match)
grep "FORM=" ascii.txt | sed 's/A/X/g; s/S/Y/g'
grep "FORM=" greek.txt | sed 's/α/X/g; s/Ω/Y/g'
grep "FORM=" octo.txt | sed 's/e0/X/g; s/Σ/Y/g'
# All produce same X/Y pattern!
```

---

## Notes on the Fano Plane and Octonions

The Fano plane (7 points, 7 lines) IS the multiplication diagram of octonions. This provides:
- Minimal closed relation algebra
- Idempotent projection behavior
- Natural folding semantics (origami axioms)
- Tetrahedral/Merkaba geometry (dual tetrahedra)

The 8-fold structure (7 imaginary units + 1 real) aligns with:
- Genesis: 8-byte HW fingerprint + 8-integer V8 topology
- Tetragrammatron: 8-tuple atom set
- Mathematics: Octonion algebra closure

---

## Development Workflow

### Adding New Features

1. Determine subsystem (OS, Genesis, or Integration)
2. Preserve invariants (check the "Development Principles" section)
3. Update relevant docs in `docs/` or subsystem `docs/`
4. Test with existing examples
5. Update CHANGELOG.md

### Working with Archives

Old versions and experiments are in `dev/`:
- `dev/archive/` - Previous implementations (reference only)
- `dev/research/` - Analysis documents and design notes
- `dev/experiments/` - Prototypes and explorations

**Do not modify archive files** - they're frozen for reference.

---

## Support

- Report issues: Create issue with clear reproduction steps
- Documentation: Check `docs/guides/` first
- Philosophy: Read `docs/Tetragrammatron-OS.md` (white paper)
- Contributing: See `docs/CONTRIBUTING.md`

---

## Summary

You're working with two peer systems:

**Tetragrammatron-OS**: Computation based on relations (not numbers)
**Genesis Protocol**: Coordination based on structure (not content)

The integration enables distributed, provenance-verified, mathematically-validated computation that works across platforms (Linux, Android, ESP32, browser) without centralized authority.

**Key insight**: Numbers, clocks, and addresses are **derived projections**, never canonical truth. This makes the system portable, verifiable, and future-proof.
