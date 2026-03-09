# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Tetragrammatron-OS is a **relational computational substrate** that replaces numeric-centric OS abstractions with **pure relation algebra**, **geometric invariants**, and **projection-based compatibility layers**. This is not a Linux distribution, VM, or configuration language—it is a foundational model for computation itself.

### Core Philosophy

**Relations precede numbers.** Everything treated as a number elsewhere (addresses, file descriptors, clocks, device IDs) is a **derived, ephemeral view** over a canonical relational graph. Numbers are allowed only as:
- Derived views from queries
- Temporary computation artifacts
- Session-local projections (never canonical identity)

The system operates in a **pre-metric geometric space** where orientation, direction, adjacency, and containment exist without numeric quantification.

## Build Commands

### Bootstrap Toolchain (A_then_B - Recommended)

Build the seed assembler and disassembler:
```bash
cd tetragrammatron-os_bootstrap_A_then_B
make
```

Assemble CANASM source to CANB bytecode:
```bash
./bin/canasm0 src/canasm/seed/canasm1.canasm -o /tmp/demo.canb
```

Disassemble CANB bytecode back to text:
```bash
./bin/canasm0-disasm /tmp/demo.canb > /tmp/demo.canasm
```

### WebAssembly VM Build

Build the CANVM interpreter for browser environments:
```bash
cd tetragrammatron-os_bootstrap_A_then_B
clang --target=wasm32 -O2 -nostdlib \
  -Wl,--no-entry -Wl,--export-all -Wl,--allow-undefined \
  -o web/demo/canvm.wasm src/canb/wasm/canvm_wasm.c
```

### Web Demo

Run the browser-based demo with Fano visualization:
```bash
cd tetragrammatron-os_bootstrap_A_then_B/web/demo
python3 -m http.server 8000
# Open http://localhost:8000
```

## Architecture

### Three-Layer Architecture

1. **CANASM** (Assembler Layer)
   - `canasm0.c`: Seed assembler emitting CANB v1 containers
   - `canasm0_disasm.c`: Disassembler for verification
   - Input: `.canasm` text files with `@atoms` and `@code` sections
   - Output: `.canb` binary containers

2. **CANB** (Bytecode Layer)
   - Binary container format with 4-byte magic `CANB`
   - ULEB128-encoded atom table (symbolic identifiers)
   - Platform-independent bytecode stream
   - Core primitives: `ATOM`, `EDGE`, `PROJ_FANO`, `HALT`

3. **CANVM** (Execution Layer)
   - Stack-based VM with graph/relation store
   - Operations are **relation traversals**, not numeric computations
   - Deterministic execution (same input → same CANB bytes)
   - Trace/event hooks for visualization

### CAN-ISA to CANB Bridge

Higher-level operations (fold axioms, meet/join, canonicalization) map to primitive CANB operations:

- `FOLD_AXIOM_k` → `PUSH ATOM:axiom:k` + `PUSH ATOM:crease:<id>` + `EDGE_ADD` + `PROJ_FANO`
- `MEET(GCD)` → `EDGE_ADD` + `PROJ_FANO` (barrier hook)
- `JOIN(LCM)` → `EDGE_ADD` + `PROJ_FANO` (barrier hook)
- `CANON` → `PROJ_FANO` (normalize-as-projection hook)

This separation allows semantic evolution without rewriting the transport layer.

### The Canonical 8-Tuple

The system uses exactly 8 normalized semantic atoms (aligned with octonion/Fano plane structure):
- `accept`, `alphabet`, `left`, `right`, `delta`, `start`, `state`, `reject`

These appear as atoms in CANB containers and node labels in visualizations. This is NOT numerology—it's closure algebra: 7 fundamental relations + 1 identity, providing minimal non-associative but closed relation algebra.

### Hardware and POSIX as Projections

**Hardware (RFC-0016):** Hardware capabilities are represented as atoms + relations, not numeric IDs. Serial numbers, MACs, bus addresses are observations (non-canonical). Capabilities are names (`cap:net:wifi`, `cap:isa:armv7m`) connected via `EXPOSES` edges.

**POSIX (RFC-0017):** Filesystem operations are projections:
- Paths = repeated `CONTAINED_IN` traversal (queries, not stored)
- Hard links = multiple `BINDS_NAME` edges to same inode
- File descriptors = ephemeral session-local tokens
- Mounts = `OVERLAYS` relations (resolution rules, not objects)

## File Organization

### Active Bootstrap Implementations

- `tetragrammatron-os_bootstrap_A_then_B/`: **Primary implementation** with assembler + disassembler
  - `src/canasm/seed/`: Seed toolchain (C99, portable)
  - `src/canb/spec/CANISA_CANB_BRIDGE.md`: Bridge semantics (critical reference)
  - `src/canb/wasm/`: WebAssembly VM implementation
  - `web/demo/`: Browser demo with SVG Fano projection

- `tetragrammatron-os_bootstrap/`: Earlier bootstrap (assembler only)

### Documentation

- `Tetragrammatron-OS.md`: Executive summary / white paper
- `dev-docs/`: RFCs and implementation specs
  - `CANB v0 implementation.md`: Complete C implementation reference
  - `RFC-0016`: Hardware-as-Relations spec
  - `RFC-0017`: POSIX-as-Projection spec
  - `CANISA_CANB_BRIDGE.md`: Macro expansion rules
- `docs/CONTRIBUTING.md`: **Critical philosophical guide** for contributors
- `archive/`: Previous iterations and research artifacts

## Development Principles

### Invariants to Preserve

Before any change, verify:
1. **No numeric indices as canonical truth** - atom indices are compression; canonical identity is atom bytes
2. **Deterministic graph operations** - `EDGE_FIND_*` uses minimal canonical atom byte ordering
3. **Projection is non-semantic** - `PROJ_FANO` emits events but does not mutate the relation graph
4. **Pure relations** - all semantics are edges between atoms

### What NOT to Do

Do not introduce changes that:
- Treat numbers as canonical identity or truth
- Collapse relations into metrics
- Assume clocks define correctness
- Encode meaning into hardware properties
- Treat POSIX, Nix, or any OS as foundational
- Add abstraction without observational grounding

### Semantic System, Not Codebase

You are not adding features—you are refining a worldview made executable. As stated in CONTRIBUTING.md:
- Prefer relations over structures
- Prefer projection over translation
- Prefer observation over assumption
- Prefer idempotence over optimization
- Prefer reversibility over speed

If something cannot be reversed, it must be justified.

## Key Implementation Patterns

### CANASM Assembly Format

```
@atoms
  accept
  alphabet
  left
@end

@code
  ATOM accept
  ATOM alphabet
  EDGE           ; Creates accept → alphabet edge
  PROJ_FANO      ; Visualization barrier
  HALT
@end
```

### Atom Ordering (Determinism)

Graph iteration must be deterministic. The `canb_atom_cmp` function provides stable ordering:
1. Synthetic atoms (session FDs) sort after file atoms
2. Primary: lexicographic byte comparison
3. Tie-break: shorter length first
4. Final: atom kind, then index

### Session vs Canonical

- **Canonical**: Atoms in CANB file, relation graph edges
- **Session-local**: FD tokens (synthetic atom indices >= `atom_count`), CWD state
- FD projection: `inode → fd_alloc() → session_fd_atom`, reversible via `fd_lookup()`

## Target Platforms

The system is designed for:
- Embedded: ESP32, RP2040 (Pico)
- Mobile: Android/Termux environments
- Browser: WASM + SVG visualization
- Decentralized: Multi-node without consensus clocks
- Future: AR/VR geometry reasoning

Platform-specific numeric details (frequencies, RAM sizes, temperatures) are observations only—never canonical identity.

## Testing Approach

When adding new functionality:
1. Write minimal `.canasm` programs exercising new operations
2. Assemble with `canasm0`, disassemble with `canasm0-disasm`, verify round-trip
3. Check determinism: same input must produce identical CANB bytes
4. Verify projection separation: trace events should not affect graph state
5. Test with minimal atom tables (8-16 atoms) before scaling

## Notes on the Fano Plane and Octonions

The multiplication diagram of octonions IS the Fano plane (7 points, 7 lines). This provides:
- Minimal closed relation algebra
- Idempotent projection behavior
- Natural folding semantics (origami axioms)
- Tetrahedral closure → dual tetrahedra (Merkaba) geometry

The lattice visualization is optional; the algebraic closure is fundamental.
