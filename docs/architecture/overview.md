# Architecture Overview

## Introduction

The Tetragrammatron Project implements a **relational computational substrate** where computation is based on graph algebra rather than numeric primitives. This document provides a high-level architectural overview.

## Core Philosophy

### Relations Precede Numbers

Traditional computing:
```
Address → Value (numbers are primitive)
```

Tetragrammatron approach:
```
Atom → Edge → Atom (relations are primitive)
Numbers are derived projections
```

### Three-Layer Model

**Layer 1: Ball (Hardware)**
- Physical constraints
- Platform-specific observations
- Non-canonical, discardable

**Layer 2: Sphere (Semantics)**
- Relational invariants
- Platform-independent truth
- Canonical, preserved

**Layer 3: Projection (Interface)**
- Derived views (POSIX, hardware IDs, timestamps)
- Compatibility layers
- Idempotent: `proj(proj(x)) = proj(x)`

## System Components

### 1. Tetragrammatron-OS

**Purpose**: Relational computation substrate

**Components**:
- **CANASM** - Assembler (text → bytecode)
- **CANB** - Binary container format
- **CANVM** - Execution engine (bytecode → projections)

**Key Invariant**: Deterministic execution (same graph → same projections)

### 2. Genesis Protocol

**Purpose**: Structural provenance without authority

**Components**:
- **Two-Primitive Projection Model (TPPM)** - P₀/P₁ decomposition
- **8-dimensional fingerprinting** - HW (8 bytes) + V8 (8 integers) + FORM
- **Distributed atoms** - JSONL database with structural identity

**Key Invariant**: Symbol-agnostic (shape matters, not glyphs)

### 3. Revelation Observer

**Purpose**: Structural observation without collapse

**Components**:
- **8-pass diffusion** - Regex-only structural projection
- **Structural basis** - SHA256 fingerprint of diffused structure
- **Deterministic comparison** - Same structure → same hash

**Key Invariant**: Observation without semantic interpretation

### 4. Integration Layer

**Purpose**: Combined capabilities

**Components**:
- **Fano-gate validator** - Octonionic closure enforcement
- **Genesis control files** - `.genesis*` routing rules
- **Provenance chains** - CANB + Genesis metadata

**Key Invariant**: Mathematical validation before execution

## Data Flow

### Assembly Pipeline

```
Source (.canasm)
    ↓
  CANASM assembler
    ↓
Bytecode (.canb)
    ↓
  Fano-gate validator
    ↓
CANVM execution
    ↓
Projection output (SVG/GLB/POSIX)
```

### Provenance Pipeline

```
File system
    ↓
  Genesis probe
    ↓
Atoms database (.jsonl)
    ↓
  Genesis translate
    ↓
HW/V8/FORM fingerprint
    ↓
  Peer exchange
    ↓
Distributed sync
```

### Structural Diffusion Pipeline

```
Any file
    ↓
  Pass 0: Identity (anchor)
    ↓
  Pass 1: Alphanumeric collapse
    ↓
  Pass 2: Non-alphanumeric isolation
    ↓
  Pass 3: Boundary normalization
    ↓
  Pass 4: Repetition compression
    ↓
  Pass 5: Symmetry check (sort + uniq)
    ↓
  Pass 6: Context adjacency (sliding window)
    ↓
  Pass 7: Closure (SHA256)
    ↓
Structural basis hash
    ↓
  Comparison / Deduplication
    ↓
Structural equivalence classes
```

## Architectural Constraints

### 1. No Numeric Identity as Canonical Truth

**Prohibited**:
- Using memory addresses as stable identity
- Using array indices as canonical references
- Using timestamps as ordering primitives

**Required**:
- Atoms with symbolic names
- Relations with typed edges
- Observations tagged with source/context

### 2. Deterministic Graph Operations

**Guaranteed**:
- Same input graph → same output projections
- Stable ordering for graph queries
- Reproducible bytecode assembly

**Implementation**:
- ULEB128 encoding (canonical)
- Sorted atom tables
- Deterministic edge traversal

### 3. Projection Idempotence

**Guaranteed**:
- `proj(x)` = `proj(proj(x))`
- Projections don't mutate graph
- Multiple projections don't interfere

**Implementation**:
- Read-only graph queries
- Stateless projection functions
- No side effects in VM

### 4. Symbol Agnosticism (Genesis)

**Guaranteed**:
- P₀/P₁ shape is structural truth
- Symbols are presentation layer
- Different symbols → same FORM skeleton

**Implementation**:
- Environment variable customization
- Alternation pattern preservation
- Symbol-independent comparison

### 5. Regex-Only Diffusion (Revelation Observer)

**Guaranteed**:
- Structure revealed, not meaning imposed
- No AST construction or semantic parsing
- Observation without collapse (measurement preserves state)
- Deterministic basis (same structure → same hash)

**Implementation**:
- 8-pass regex transformations only
- No unbounded computation (finite automata)
- No global context or memory
- SHA256 closure for irreversible projection

**Why regex-only?**:
- **Finite** - No unbounded computation
- **Local** - No global context needed
- **Memoryless** - No hidden state
- **Non-hallucinatory** - Cannot invent meaning

## Mathematical Foundation

### Fano Plane (Octonions)

The 7-point Fano plane is the multiplication table of octonions:

```
7 points = 7 imaginary units (e₁-e₇)
7 lines = 7 multiplication triads
+ 1 real unit (e₀) = 8-dimensional algebra
```

**Usage in Tetragrammatron**:
- Canonical 8-tuple atom set
- Fano-gate validation (closure checking)
- Merkaba projection (3D visualization)

### Octonionic Closure

A subset S of octonion units is **closed** if:
```
∀ a,b ∈ S: if line(a,b) = {a,b,c}, then c ∈ S
```

**Translation to CANB**:
- If program uses atoms A and B
- And A,B lie on Fano line with atom C
- Then program must also use atom C

**Enforcement**: Fano-gate validator rejects programs that violate closure

### Two-Primitive Projection Model (TPPM)

All content decomposes into:
```
P₀ = Semantic mass (alphanumeric runs)
P₁ = Structure (non-alphanumeric runs)
```

**Properties**:
- Partition is complete (every byte is P₀ or P₁)
- Alternation pattern is structural invariant
- Symbols can be substituted without changing structure

**Example**:
```
ASCII:    AAAA SSS AAAAA S AAAA SSSSSSSS
Greek:    αααα ΩΩΩ ααααα Ω αααα ΩΩΩΩΩΩΩΩ
Octonion: e0e0e0e0 ΣΣΣ e0e0e0e0e0 Σ e0e0e0e0 ΣΣΣΣΣΣΣΣ
```

All three have identical FORM skeleton: `AAAASSSAAAAASAAAASSSSSSSS`

## Deployment Topology

### Single-Node (Local Development)

```
[Developer Machine]
    ├── tetragrammatron-os/ (build & execute)
    ├── genesis-protocol/ (fingerprint)
    └── integration/ (validate)
```

**Use case**: Local development, testing, experimentation

### Multi-Node (Distributed Sync)

```
[Node A]                    [Node B]
    ├── atoms-A.jsonl           ├── atoms-B.jsonl
    └── .canb programs          └── .canb programs
         ↓                              ↓
    Genesis exchange ←→ Genesis exchange
         ↓                              ↓
    Reconcile differences
         ↓
    Sync structural atoms
```

**Use case**: Peer-to-peer code sharing without Git

### Federated (Multi-Platform)

```
[Linux Server]     [Android/Termux]     [ESP32]      [Browser/WASM]
    CANVM              CANVM              CANVM         CANVM
      ↓                  ↓                  ↓             ↓
  POSIX proj        POSIX proj         Bare metal    GLB/SVG proj
```

**Use case**: Platform-agnostic execution with local projections

## Performance Characteristics

### Assembly

- **Complexity**: O(n) where n = source lines
- **Determinism**: Same source → identical bytecode (MD5 match)
- **Overhead**: Minimal (C99, no dependencies)

### Validation

- **Complexity**: O(7 × m) where m = atom count
- **Worst case**: Full 7-atom program = 7 Fano lines × 7 atoms = 49 checks
- **Typical**: Most programs < 5 atoms = ~25 checks

### Execution

- **Complexity**: O(e) where e = edge count in graph
- **Graph operations**: Stack-based, deterministic ordering
- **Projection**: O(p × v) where p = projection type, v = vertex count

### Genesis Fingerprinting

- **Complexity**: O(f) where f = file size
- **Single file**: < 1ms for typical source files
- **Database probe**: O(n × f) where n = file count
- **Typical**: 16-file codebase = ~50ms total

### Structural Diffusion (Revelation Observer)

- **Complexity**: O(8 × f) where f = file size
- **Pass count**: Fixed 8 passes (constant factor)
- **Single file**:
  - < 10 KB: < 100ms
  - 10-100 KB: 100-500ms
  - 100KB-1MB: 0.5-2s
- **Deterministic**: Same input → identical hash every time
- **Parallelizable**: Can process multiple files in parallel

## Security Model

### Threat Model

**Assumptions**:
- CANB bytecode may come from untrusted sources
- Remote peers may be malicious
- Execution environment may be compromised

**Guarantees**:
- Fano-invalid programs rejected before execution
- Deterministic execution (no hidden state)
- No numeric primitives to exploit (no buffer overflows via addresses)

### Trust Boundaries

**Trust Required**:
- Assembler correctness (canasm0.c is trusted)
- Disassembler correctness (canasm0_disasm.c is trusted)
- Validator correctness (canb-fano-validator.sh is trusted)

**No Trust Required**:
- Remote peer reputation
- Centralized authority
- Network infrastructure
- Timestamps or clocks

### Validation Chain

```
Source → Assemble → Validate → Execute
  ↓          ↓          ↓          ↓
Audit    Determinism  Fano-gate  Projection
```

**Each step preserves invariants**:
1. Assembly: Produces canonical bytecode
2. Validation: Enforces mathematical constraints
3. Execution: Deterministic graph traversal
4. Projection: Idempotent, read-only views

## Extension Points

### Adding New Projections

1. Define projection function: `Graph → Output`
2. Ensure idempotence: `proj(proj(x)) = proj(x)`
3. Implement in VM or external tool
4. Document output format

**Example**: Add 3D projection
- Input: Graph with Fano-validated atoms
- Output: GLB file (glTF binary)
- Location: `web/demo/fano_merkaba.js`

### Adding New Validators

1. Define validation predicate: `Graph → {Valid, Invalid}`
2. Implement as shell script or C program
3. Place in `integration/validators/`
4. Document usage in `integration/README.md`

**Example**: Type system validator
- Input: CANB program
- Check: Type consistency across edges
- Output: VALID or INVALID with type error

### Adding New Genesis Symbols

1. Define P₀/P₁/WS symbols
2. Set environment variables
3. Use `genesis-enhanced.sh`
4. Verify FORM skeleton matches

**Example**: Add mathematical symbols
```bash
GENESIS_P0_SYMBOL=∀ GENESIS_P1_SYMBOL=∃ \
  ./genesis-enhanced.sh translate file.c
```

## Future Architecture

### Planned Components

**Hardware Probing** (RFC-0016):
- Query hardware capabilities as graph
- Project to VM semantic boundary
- Enable hardware-aware scheduling

**POSIX Emulation** (RFC-0017):
- Implement POSIX as projection layer
- Enable running on bare metal (ESP32)
- Maintain compatibility without POSIX dependency

**Distributed VM**:
- Multi-node graph execution
- Atom exchange via Genesis
- Consensus via Fano-gate validation

### Research Directions

**Formal Verification**:
- Lean proofs for core algorithms
- Verified compiler (CANASM → CANB)
- Verified validator (Fano-gate)

**Performance Optimization**:
- JIT compilation for hot paths
- GPU acceleration for graph queries
- SIMD for projection rendering

**Language Extensions**:
- Higher-level abstractions (functions, modules)
- Type system (enforce edge constraints)
- Macro system (CANASM expansion)

## Conclusion

The Tetragrammatron architecture is built on **relations as primitives** rather than numbers. This enables:

- Platform-agnostic computation
- Provenance-verified execution
- Mathematically-validated programs
- Distributed coordination without authority
- Structural observation without semantic interpretation

The three-layer model (Ball/Sphere/Projection) separates **physical constraints** from **semantic invariants** from **compatibility interfaces**, allowing the system to run anywhere while preserving correctness.

The Revelation Observer adds a fourth dimension: **observation without collapse** - viewing structural emergence through regex-only diffusion without asserting meaning.

---

**See also**:
- [System Design](system-design.md) - Detailed component interactions
- [Data Structures](data-structures.md) - Graph representation
- [Projection Model](projection-model.md) - Ball/Sphere/Projection theory
- [Revelation Observer Guide](../guides/revelation-observer.md) - Structural diffusion deep dive
- [Getting Started](../guides/getting-started.md) - Practical usage
