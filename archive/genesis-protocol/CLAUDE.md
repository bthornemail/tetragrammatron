# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

GENESIS-PROTOCOL is a minimal formal system for defining **structure before interpretation**. It separates:
- **What exists** (hardware/provenance)
- **What means** (software/semantics)
- **What projects** (syntax/view)

The core principle: **Only what responds may remain.**

## Two-Primitive Projection Model (TPPM)

Everything in this system reduces to exactly two primitives:

- **P₀ (Semantic Mass)**: `[[:alnum:]]+` — alphanumeric runs representing semantic content
- **P₁ (Structural Projection)**: `[^[:alnum:]]+` — non-alphanumeric runs representing structure

All valid expressions reduce to alternation: `(P₀ ∘ P₁)* ∘ P₀?` or `(P₁ ∘ P₀)* ∘ P₁?`

Equivalence is determined by:
1. Same primitive alternation sequence
2. Same run-length vectors after projection

This is defined formally in `docs/RFC-0001-TPPM.md` at Layer −0 (pre-semantic invariant).

## Layer Model

The system operates on a strict layer hierarchy:

| Layer | Role | Artifact | Description |
|-------|------|----------|-------------|
| −0 | Constraint | RFC-0001-TPPM | Pre-system invariant (hardware/byte level) |
| 0 | Index | INDEX.org | Positional identity and addressability |
| 0′ | Orientation | README.org, GLOSSARY.org | Public-facing documentation |
| 1 | Conversation | CONVERSATION.org | Temporal emergence and process |
| 2 | Manifesto | MANIFESTO.org | Intent and values |
| 3 | Revelation | REVELATION.org | Personal synthesis |

**Critical rule**: Layers only depend on lower layers, never higher ones.

## Common Commands

### Using genesis-protocol.sh (RFC-0002 Reference Implementation)

The RFC-0002 reference implementation provides the complete workflow:

```bash
# Initialize a GENESIS repository
./genesis-protocol.sh init

# Run complete workflow (generate → contemplate → validate)
./genesis-protocol.sh workflow

# Individual layer commands:
./genesis-protocol.sh generate    # Layer 0: Observe filesystem
./genesis-protocol.sh contemplate # Layer 1: Project to .org views
./genesis-protocol.sh interpret   # Layer 2: Apply schema/scheme
./genesis-protocol.sh translate ATOM [SCHEME]  # Layer 3: Emit artifacts
./genesis-protocol.sh validate    # Layer 7: Reconcile with reality

# Check status
./genesis-protocol.sh status
```

See [docs/IMPLEMENTATION.md](docs/IMPLEMENTATION.md) for complete usage guide.

### Using genesis.sh (TPPM Implementation)

The original TPPM implementation provides detailed fingerprinting:

```bash
# Probe a directory tree and generate atoms.jsonl
./genesis.sh probe [ROOT] [OUT.jsonl]

# Compile a file with GENESIS metadata wrapper
./genesis.sh compile INFILE [OUT.org]

# Extract original payload from compiled file
./genesis.sh decompile IN.org [OUTFILE]

# Show hardware/software/form fingerprint of a file
./genesis.sh translate FILE

# Generate 8-dimensional topology probe
./genesis.sh mux8 FILE
```

### Working with genesis-protocol/ subdirectory

The `genesis-protocol/` directory contains the multi-layer Scheme/C implementation:

```bash
cd genesis-protocol/

# Tangle genesis.org to generate all layer files
./genesis.sh
# (This runs emacs --batch with org-babel-tangle-file)

# Layer files are generated in:
# - genesis/layer.0/ through genesis/layer.7/
# - tools/
# - creation/, logos/, revelation/
```

### File Classification System

The system uses three control files for file selection:

- **`.genesisignore`**: Files in this category are UNKNOWN-UNKNOWN (dark matter) — excluded from processing
- **`.genesisinclude`**: Files in this category are KNOWN-KNOWN (light matter) — included for processing
- **`.genesis`**: Functor table mapping ignore/include combinations to processing rules

Files are classified as:
- **UU** (Unknown-Unknown): Ignored patterns, no processing
- **KU** (Known-Unknown): Deferred processing
- **KK** (Known-Known): Full processing with default rules

## Architecture

### Hardware/Software Duality

Every file has two projections:

1. **Hardware Constraint (HW)**: 8-byte fingerprint from POSIX `stat()` fields
   - Format: `XX:XX:XX:XX:XX:XX:XX:XX` (hex bytes)
   - Derived from: inode, device, mode, uid, gid, size, mtime, ctime
   - Represents provenance/physical identity

2. **Software Form (V8)**: 8-integer topology vector
   - Captures structural properties: run counts, transitions, nesting depth, separators
   - Preserves alphanumeric semantics while abstracting symbols

3. **FORM**: Skeleton projection mapping characters to:
   - `A` = alphanumeric
   - `_` = whitespace
   - `S` = separator/symbol

### Meta-Tags (Shape, Not Symbols)

Meta-tags are defined structurally as `P₁ ∘ P₀ ∘ P₁`:

```
[[term]]      ← Org-mode link
💻term💻      ← Emoji wrapper
<term>        ← XML-like tag
::term::      ← Custom delimiter
```

All are **structurally equivalent** — symbols don't matter, only shape matters.

### Matter Chain Model (Front-matter + Aftermatter)

Files can be wrapped with provenance metadata:

- **Front-matter**: L0 record (GENESIS-CID from POSIX stat) + L0CID hash
- **Payload**: Original file content (canonical form)
- **Aftermatter**: L1 fingerprint (hash over canonical payload)

This creates an immutable chain where mutations force new aftermatter while preserving provenance.

## Implementation Notes

### Literate Programming with Org-Mode

The primary source file is `genesis-protocol/genesis.org` which uses Emacs Org-mode literate programming:

- Code blocks are marked with `#+BEGIN_SRC <lang> :tangle <output-path>`
- Running `./genesis.sh` tangles all code to proper locations
- This ensures single-source-of-truth for the layer implementation

### Language Choices

- **Scheme**: Primary implementation language for layers 0-7 (functional, minimal dependencies)
- **C**: Optional performance-critical helpers (POSIX stat access)
- **POSIX sh**: Reference implementation in root `genesis.sh` (maximum portability)

### No Dependencies Required

The system is designed to work with minimal tooling:
- POSIX shell (sh)
- AWK (for text processing)
- sed (for transformations)
- Basic POSIX utilities (stat, find)
- Optional: Emacs (for tangling org files), Guile/Chicken (for running Scheme)

## Important Constraints

1. **No Semantic Interpretation at Layer −0**: The TPPM model operates *before* meaning, syntax, or execution
2. **Preservation of Alphanumeric Identity**: Software form MUST preserve `[[:alnum:]]` runs
3. **Non-Authority Clause**: Org-mode, Markdown, emojis are all *views* — GENESIS remains valid if any syntax is changed
4. **Frozen Foundation**: The core layer model and TPPM spec are intentionally frozen and should not be modified
5. **File Selection Hygiene**: Binary files, build artifacts, and version control metadata are excluded via `.genesisignore`

## Working with This Codebase

### To Add a New Feature

1. Determine which layer it belongs to (0-7 for core, or logos/creation/revelation for extensions)
2. Add implementation to appropriate section in `genesis-protocol/genesis.org`
3. Run `./genesis.sh` to tangle to output files
4. Test the generated layer files

### To Understand the Theory

Read in this order:
1. `docs/RFC-0002-GENESIS-PROTOCOL.md` — Full GENESIS Protocol specification
2. `docs/RFC-0001-TPPM.md` — Two-Primitive Projection Model (foundational)
3. `docs/RFC-GENESIS-GLOSSARY-0001.md` — Authoritative glossary of terms
4. `GENESIS.org` — Canonical declaration
5. `README.org` — Public orientation
6. `GLOSSARY.org` — Public-facing language contract (brief)
7. `docs/AGENTS.md` — Consensus theory and simulation architecture

### To Probe a File or Directory

```bash
# See all fingerprints of a file
./genesis.sh translate path/to/file

# Generate JSONL database of all tracked files
./genesis.sh probe . atoms.jsonl

# Wrap a file with GENESIS metadata
./genesis.sh compile input.txt output.org
```

## Repository Structure

```
.
├── genesis.sh              # Root POSIX reference implementation
├── GENESIS.org             # Canonical system declaration (Layer 0)
├── INDEX.org               # Orientation & addressability
├── README.org              # Public-facing documentation
├── GLOSSARY.org            # Language contract
├── CONVERSATION.org        # Temporal emergence (Layer 1)
├── MANIFESTO.org           # Intent & values (Layer 2)
├── REVELATION.org          # Personal synthesis (Layer 3)
├── docs/                   # Extended documentation and RFCs
│   ├── RFC-0002-GENESIS-PROTOCOL.md  # Full GENESIS Protocol spec
│   ├── RFC-0001-TPPM.md   # Formal two-primitive model spec
│   ├── RFC-GENESIS-GLOSSARY-0001.md  # Authoritative glossary
│   ├── AGENTS.md          # Consensus simulation theory
│   └── *.md               # Appendices and extensions
└── genesis-protocol/       # Multi-layer Scheme/C implementation
    ├── genesis.sh         # Org-mode tangling script
    ├── genesis.org        # Literate source for all layers
    ├── genesis/           # Generated layer implementations
    │   ├── layer.0/       # Existence (POSIX facts only)
    │   ├── layer.1/       # Selection (ignore/include)
    │   ├── layer.2/       # Identity (content hashing)
    │   ├── layer.3/       # Relation (structural analysis)
    │   └── layer.4-7/     # Higher abstractions
    ├── tools/             # Utilities (genesis-tangle.sh, etc.)
    ├── logos/             # Optional: logic extensions
    ├── creation/          # Optional: generative systems
    └── revelation/        # Optional: personal workspace
```

## Philosophy

This is:
- A boundary definition, not a belief system
- A constraint framework, not an ideology
- A method of alignment, not authority
- A system for provenance, not power

This is NOT:
- A theology
- A governance model
- A truth oracle
- A replacement for human judgment

When working in this codebase, respect the separation of structure from interpretation, and preserve the layered architecture.
