# Revelation Observer Guide

## Introduction

The **Revelation Observer** is a projection surface for viewing **structural emergence** through multi-pass regex diffusion without semantic interpretation.

> "A system for viewing structural emergence without asserting meaning."

This guide will teach you how to use the Revelation Observer to:
- Understand file structure without parsing
- Compare files structurally (not semantically)
- Track structural evolution across versions
- Generate deterministic structural fingerprints

## Core Philosophy

### What It Is

The Revelation Observer:
- ✅ **Projects structure** through 8 regex passes
- ✅ **Reveals invariants** without interpretation
- ✅ **Shows diffusion** stages transparently
- ✅ **Enables comparison** without judgment
- ✅ **Preserves emergence** (measurement without collapse)

### What It Is NOT

The Revelation Observer does **not**:
- ❌ Parse code semantically
- ❌ Build ASTs or syntax trees
- ❌ Interpret meaning
- ❌ Provide conclusions
- ❌ Collapse observations

### Why Regex-Only?

**Regex is**:
- **Finite** - No unbounded computation
- **Local** - No global context needed
- **Memoryless** - No hidden state
- **Non-hallucinatory** - Cannot invent meaning

> "Regex doesn't compute meaning — it reveals invariants."

## The 8-Pass Model

Each pass transforms the input through a specific regex operation:

```
Pass 0: Identity (anchor)
  ↓
Pass 1: Alphanumeric collapse (software projection)
  ↓
Pass 2: Non-alphanumeric isolation (syntax projection)
  ↓
Pass 3: Boundary normalization
  ↓
Pass 4: Repetition compression
  ↓
Pass 5: Symmetry check (sort + uniq)
  ↓
Pass 6: Context adjacency (sliding window)
  ↓
Pass 7: Closure (basis promotion via SHA256)
```

**Properties**:
- Each pass is **idempotent** (running twice = running once)
- Passes are **order-sensitive** (Pass N depends on Pass N-1)
- Result is **deterministic** (same input → same output)

## Installation

The Revelation Observer is included in the Tetragrammatron distribution:

```bash
cd tetragrammatron/revelation-observer/
ls bin/
# genesis-diffuse.sh
```

No build step required - it's a POSIX shell script.

## Basic Usage

### Get Structural Basis

```bash
cd revelation-observer/

# Diffuse a file to its structural basis
./bin/genesis-diffuse.sh myfile.c

# Output: SHA256 hash (structural fingerprint)
# Example: 68623252ef15db33503289a6fe603e5f207a225467ad3db0076ce2ee3014af19
```

**What this means**:
- Same structure → same hash
- Different structure → different hash
- No semantic interpretation involved

### Verbose Mode

See all 8 passes:

```bash
./bin/genesis-diffuse.sh --verbose myfile.c
```

**Output**:
```
=== Pass 0: Identity (anchor) ===
Length: 6865 bytes

=== Pass 1: Alphanumeric collapse (software projection) ===
Sample: CANASM0 Seed assembler for CANB v1
Length: 6863 bytes

=== Pass 2: Non-alphanumeric isolation (syntax projection) ===
Sample: /* — - : , -
Length: 6865 bytes

=== Pass 3: Boundary normalization ===
Length: 2859 bytes

=== Pass 4: Repetition compression ===
Length: 2778 bytes

=== Pass 5: Symmetry check (sort + uniq) ===
Unique lines: 133

=== Pass 6: Context adjacency (sliding window) ===
Length: 2795 bytes

=== Pass 7: Closure (basis promotion) ===
Basis hash: 68623252ef15db33503289a6fe603e5f207a225467ad3db0076ce2ee3014af19

=== Diffusion Complete ===
```

### Keep Temporary Files

Inspect intermediate passes:

```bash
./bin/genesis-diffuse.sh --keep-temp myfile.c
```

Temporary files are preserved in `$HOME/tmp/genesis-diffuse-*/`:

```
$HOME/tmp/genesis-diffuse-abc123/
├── input      # Original file
├── pass0      # Identity
├── pass1      # Alphanumeric only
├── pass2      # Non-alphanumeric only
├── pass3      # Normalized boundaries
├── pass4      # Compressed repetitions
├── pass5      # Sorted & unique
├── pass6      # Context adjacency
└── pass7      # Final basis hash
```

## Understanding the Passes

### Pass 0: Identity

**Purpose**: Anchor - preserve original

**Operation**: `cat file`

**Example**:
```c
Input:  int main() { return 0; }
Output: int main() { return 0; }  (unchanged)
```

### Pass 1: Alphanumeric Collapse

**Purpose**: Software projection (semantic mass)

**Operation**: Replace all non-alphanumeric with space

**Example**:
```c
Input:  int main() { return 0; }
Output: int main     return 0
```

**Why**: Captures P₀ (semantic mass) from Genesis TPPM model

### Pass 2: Non-Alphanumeric Isolation

**Purpose**: Syntax projection (structure)

**Operation**: Replace all alphanumeric with space

**Example**:
```c
Input:  int main() { return 0; }
Output:         () {        ; }
```

**Why**: Captures P₁ (structure) from Genesis TPPM model

### Pass 3: Boundary Normalization

**Purpose**: Collapse whitespace boundaries

**Operation**: Multiple spaces → single space

**Example**:
```
Input:  ( )    {        ; }
Output: ( ) { ; }
```

### Pass 4: Repetition Compression

**Purpose**: Remove redundant repetition

**Operation**: Consecutive identical chars → single

**Example**:
```
Input:  aaaa bbbb {{{{
Output: a b {
```

### Pass 5: Symmetry Check

**Purpose**: Find order-independent patterns

**Operation**: Sort lines + unique

**Example**:
```
Input:  { ; }
        ( )
        { ; }
Output: ( )
        { ; }
```

**Why**: Reveals structural symmetries independent of order

### Pass 6: Context Adjacency

**Purpose**: Capture local context

**Operation**: Insert `|` between adjacent characters

**Example**:
```
Input:  abc
Output: a|b|c
```

**Why**: Creates bigram view - local relationships without global position

### Pass 7: Closure (Basis Promotion)

**Purpose**: Final projection to basis

**Operation**: SHA256 hash of Pass 6 output

**Example**:
```
Input:  <pass6 output>
Output: 68623252ef15db33503289a6fe603e5f207a225467ad3db0076ce2ee3014af19
```

**Why**: Irreversible projection to stable, deterministic fingerprint

## Common Tasks

### Task 1: Compare Two Files

```bash
cd revelation-observer/

# Get basis for each file
basis_a=$(./bin/genesis-diffuse.sh file_a.c)
basis_b=$(./bin/genesis-diffuse.sh file_b.c)

# Compare
if [ "$basis_a" = "$basis_b" ]; then
  echo "Structurally identical"
else
  echo "Structurally different"
  echo "  File A: $basis_a"
  echo "  File B: $basis_b"
fi
```

### Task 2: Find Structurally Similar Files

```bash
# Diffuse all C files
for f in *.c; do
  basis=$(./bin/genesis-diffuse.sh "$f")
  echo "$basis $f"
done | sort

# Files with same basis are structurally equivalent
```

**Output**:
```
68623252... canasm0.c
68623252... canasm0_copy.c   ← Same structure!
f956a9c3... canasm0_disasm.c
```

### Task 3: Track Structural Evolution

```bash
# Diffuse multiple versions
./bin/genesis-diffuse.sh v1/program.c > basis_v1.txt
./bin/genesis-diffuse.sh v2/program.c > basis_v2.txt
./bin/genesis-diffuse.sh v3/program.c > basis_v3.txt

# Compare evolution
echo "v1→v2: $(diff -q basis_v1.txt basis_v2.txt && echo 'unchanged' || echo 'changed')"
echo "v2→v3: $(diff -q basis_v2.txt basis_v3.txt && echo 'unchanged' || echo 'changed')"
```

### Task 4: Batch Analysis

```bash
# Create structural index
echo "# Structural Basis Index" > index.txt
echo "" >> index.txt

find ../tetragrammatron-os/src -name "*.c" | while read f; do
  basis=$(./bin/genesis-diffuse.sh "$f")
  echo "$basis $(basename "$f")" >> index.txt
done

# View index
cat index.txt
```

## Examples & Demos

### Run Comparison Demo

```bash
cd revelation-observer/examples/
./demo-comparison.sh
```

**What it shows**:
- Different files → different basis hashes
- Structural uniqueness without semantic interpretation
- Equivalence testing

### Run Evolution Demo

```bash
cd revelation-observer/examples/
./demo-evolution.sh
```

**What it shows**:
- How structure evolves across versions
- Comments don't affect structure
- Implementation changes do affect structure

## Integration with Tetragrammatron

### With CANB Programs

```bash
# Diffuse CANB bytecode
./bin/genesis-diffuse.sh program.canb

# Compare CANB programs structurally
basis_a=$(./bin/genesis-diffuse.sh a.canb)
basis_b=$(./bin/genesis-diffuse.sh b.canb)

[ "$basis_a" = "$basis_b" ] && echo "Structurally equivalent CANB programs"
```

### With Genesis Protocol

**Genesis vs Revelation**:

| Aspect | Genesis | Revelation |
|--------|---------|------------|
| **Focus** | Hardware + POSIX metadata | Pure structure |
| **Output** | HW (8 bytes) + V8 (8 ints) + FORM | SHA256 basis hash |
| **Uses** | Provenance, distributed sync | Structural comparison |
| **Symbols** | Customizable (α/Ω, e₀/Σ) | N/A (regex-only) |

**Combined usage**:
```bash
# Genesis fingerprint
hw_v8=$(../genesis-protocol/genesis.sh translate file.c | grep "HW=")

# Revelation basis
basis=$(./bin/genesis-diffuse.sh file.c)

echo "Genesis (provenance): $hw_v8"
echo "Revelation (structure): $basis"
```

**Complementary roles**: Genesis tracks provenance, Revelation tracks structure.

### With Fano-Gate

```bash
# Diffuse Fano-validated programs
for prog in ../integration/validators/test-*.canb; do
  basis=$(./bin/genesis-diffuse.sh "$prog")
  name=$(basename "$prog")
  echo "$name: $basis"
done
```

**Output**:
```
test-invalid.canb: <basis-1>
test-valid.canb:   <basis-2>
test-full.canb:    <basis-3>
```

## Advanced Usage

### Custom Workflows

**Automated monitoring**:
```bash
#!/bin/sh
# monitor-structure.sh - Alert on structural changes

WATCH_DIR="../tetragrammatron-os/src"
BASELINE="baseline-basis.txt"

# Generate baseline (first run)
if [ ! -f "$BASELINE" ]; then
  find "$WATCH_DIR" -name "*.c" | while read f; do
    basis=$(./bin/genesis-diffuse.sh "$f")
    echo "$basis $f" >> "$BASELINE"
  done
  echo "Baseline created"
  exit 0
fi

# Check for changes
find "$WATCH_DIR" -name "*.c" | while read f; do
  current=$(./bin/genesis-diffuse.sh "$f")
  baseline=$(grep "$f" "$BASELINE" | cut -d' ' -f1)

  if [ "$current" != "$baseline" ]; then
    echo "ALERT: Structural change in $f"
    echo "  Was: $baseline"
    echo "  Now: $current"
  fi
done
```

**Structural deduplication**:
```bash
# Find structurally identical files
find . -name "*.c" | while read f; do
  basis=$(./bin/genesis-diffuse.sh "$f")
  echo "$basis $f"
done | sort | uniq -w 64 -D
# -w 64: First 64 chars (the hash)
# -D: Show duplicates only
```

## Troubleshooting

### Error: Permission denied (/tmp)

**Symptom**: `mktemp: failed to create directory via template '/tmp/...'`

**Cause**: `/tmp` not writable (e.g., Android/Termux)

**Solution**: Script automatically falls back to `$HOME/tmp` - no action needed

### Output looks random/meaningless

**Not an error** - This is expected!

The basis hash is a **fingerprint**, not human-readable content.

Use `--verbose` to see intermediate passes if you want to understand the transformation.

### Same content, different hash

**Check**:
1. Are files truly identical? `diff file1 file2`
2. Hidden whitespace differences? `cat -A file1`
3. Different line endings? `dos2unix file1`

If files are truly identical, hashes **will** match (deterministic guarantee).

### Different content, same hash

**Extremely unlikely** (SHA256 collision probability: ~2^-256)

If this occurs, it would be a significant cryptographic discovery!

## Performance

### Benchmarks

| File Size | Time (Approx) | Passes |
|-----------|---------------|--------|
| < 10 KB | < 100ms | 8 |
| 10-100 KB | 100-500ms | 8 |
| 100KB-1MB | 0.5-2s | 8 |
| > 1 MB | 2s+ | 8 |

**Note**: Performance depends on:
- Regex engine (GNU sed, BSD sed, etc.)
- File complexity (repetitions, unique lines)
- System load

### Optimization Tips

**For large files**:
```bash
# Use subset for quick check
head -n 1000 largefile.c | ./bin/genesis-diffuse.sh /dev/stdin
```

**For many files**:
```bash
# Parallelize with xargs
find . -name "*.c" | xargs -P 4 -I {} sh -c './bin/genesis-diffuse.sh "{}" > "{}.basis"'
# -P 4: 4 parallel processes
```

## Comparison with Other Tools

| Tool | Purpose | Method |
|------|---------|--------|
| **Revelation Observer** | Structural fingerprint | 8-pass regex diffusion |
| **Genesis Protocol** | Provenance tracking | HW + stat + P₀/P₁ |
| **git diff** | Content changes | Line-by-line comparison |
| **checksums (md5/sha)** | Exact content | Direct hash |
| **tree-sitter** | Syntax parsing | AST construction |

**Revelation Observer is unique**: Structure without semantics, regex-only, no parsing.

## Future Directions

From the original plan, potential UI implementations:

### Static HTML + WASM

- Compile diffusion to WebAssembly
- JavaScript visualization
- Runs entirely in browser

### Obsidian Canvas

- `.revelation` file format
- Visual node graph showing 8 passes
- Interactive exploration

### Terminal UI

- ncurses interface
- Real-time diff viewer
- ASCII art visualization

**Note**: These are future possibilities, not current features.

## Summary

The Revelation Observer provides:

✅ **8-pass structural diffusion** (regex-only)
✅ **Deterministic fingerprints** (same structure → same hash)
✅ **No semantic interpretation** (structure revealed, not meaning imposed)
✅ **Observation without collapse** (measurement preserves state)
✅ **Integration-ready** (works with CANB, Genesis, Fano)

**Key principle**:
> "The Observer doesn't respond. It reflects."

---

**See also**:
- [Revelation Observer README](../../revelation-observer/README.md) - Complete technical guide
- [Architecture Overview](../architecture/overview.md) - System design
- [Genesis Protocol](../../genesis-protocol/) - Complementary provenance system
- [Examples](../../revelation-observer/examples/) - Demo scripts
