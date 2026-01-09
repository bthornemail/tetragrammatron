# Creating ATOM.md with Tetragrammatron-OS and Genesis-Protocol

## Overview

`ATOM.md` describes a **post-numeric relational economy** built on the same philosophical foundation as Tetragrammatron-OS: **relations precede numbers**. This document explains how to use genesis-protocol to analyze, probe, and understand files (including ATOM.md itself) as **structural atoms** rather than numeric byte sequences.

---

## Philosophy: Files as Atoms

In Tetragrammatron-OS, every file is an **atom** in the relational graph:
- **Identity** = Structural signature (not file path)
- **Content** = Relations between character classes
- **Numbers** = Derived views (probes), never canonical

Genesis-protocol extracts these structural signatures through two complementary probes:

1. **Hardware Probe (HW)**: POSIX filesystem metadata as an 8-tuple
2. **Software Probe (V8)**: Character-level structural analysis as an 8-tuple

These probes convert files into **8-dimensional geometric descriptors** aligned with octonion algebra and Fano plane structure.

---

## Core Tools

### 1. `genesis.sh` (Standard Form)

**Location**: `tetragrammatron-os_bootstrap_A_then_B/genesis.sh`

Basic structural analysis with fixed `A`/`S`/`_` encoding:
- `A` = Alphanumeric characters (semantic mass)
- `S` = Structural separators (punctuation, operators)
- `_` = Whitespace

**Usage**:
```bash
cd tetragrammatron-os_bootstrap_A_then_B
./genesis.sh translate atom/ATOM.md
```

**Output Format**:
```
HW=<8-byte-hex-tuple>  # Hardware signature (inode, dev, mode, uid, gid, size, mtime, ctime)
V8=<8-byte-tuple>      # Software signature (total, alpha-runs, sep-runs, max-alpha, max-sep, transitions, unique-seps, newlines)
FORM=AAAA_SSS_AAAA...  # Character class sequence
```

### 2. `genesis-enhanced.sh` (Customizable Symbols)

**Location**: `tetragrammatron-os_bootstrap_A_then_B/genesis-enhanced.sh`

Extended version supporting **universal symbolic representations**:
- Greek letters (α/Ω)
- Unicode mathematical operators (e₀/∑)
- Emoji/alchemical symbols (🜂/🜃)
- Custom user-defined symbols

**Basic Usage**:
```bash
cd tetragrammatron-os_bootstrap_A_then_B
./genesis-enhanced.sh translate ../atom/ATOM.md
```

**Custom Symbol Examples**:

**Greek Alpha/Omega** (Tetragrammatron canonical):
```bash
GENESIS_P0_SYMBOL='α' GENESIS_P1_SYMBOL='Ω' GENESIS_WS_SYMBOL='∘' \
  ./genesis-enhanced.sh translate ../atom/ATOM.md
```

**Octonionic Basis**:
```bash
GENESIS_P0_SYMBOL='e₀' GENESIS_P1_SYMBOL='∑' GENESIS_WS_SYMBOL='·' \
  ./genesis-enhanced.sh translate ../atom/ATOM.md
```

**Alchemical Light/Dark**:
```bash
GENESIS_P0_SYMBOL='🜂' GENESIS_P1_SYMBOL='🜃' GENESIS_WS_SYMBOL='○' \
  ./genesis-enhanced.sh translate ../atom/ATOM.md
```

---

## Understanding the 8-Tuple Probes

### Hardware Probe (HW)

Maps POSIX metadata to 8 bytes (mod 256):

| Byte | Field   | Meaning                          |
|------|---------|----------------------------------|
| b0   | Inode   | File identity on filesystem      |
| b1   | Device  | Storage device ID                |
| b2   | Mode    | Permission bits                  |
| b3   | UID     | User ID                          |
| b4   | GID     | Group ID                         |
| b5   | Size    | File size in bytes               |
| b6   | Mtime   | Modification timestamp           |
| b7   | Ctime   | Creation/status change timestamp |

**Example Output**:
```
HW=4E:00:81:F3:F3:E6:28:28
```

**Interpretation**:
- `4E` = Inode (mod 256)
- `00` = Device
- `81` = Mode (permissions)
- `F3` = UID (user 243)
- `F3` = GID (group 243)
- `E6` = Size (230 mod 256 = 7494 bytes)
- `28` = Mtime
- `28` = Ctime

### Software Probe V8

Analyzes character-level structure without semantic parsing:

| Byte | Field         | Meaning                                  |
|------|---------------|------------------------------------------|
| v0   | Total chars   | Total character count (mod 256)          |
| v1   | Alpha runs    | Number of alphanumeric sequences         |
| v2   | Sep runs      | Number of separator/punctuation runs     |
| v3   | Max alpha     | Longest alphanumeric run                 |
| v4   | Max sep       | Longest separator run                    |
| v5   | Transitions   | Character class transitions (A↔S)        |
| v6   | Unique seps   | Distinct separator characters            |
| v7   | Newlines      | Total newline count                      |

**Example Output**:
```
V8=230,945,944,27,4,1888,17,305
```

**Interpretation** (for ATOM.md):
- 7494 total chars (230 mod 256)
- 945 alphanumeric runs (words/identifiers)
- 944 separator runs
- Longest word: 27 characters
- Longest separator run: 4 characters
- 1888 transitions between classes
- 17 unique separator types
- 305 lines

### Structural Form (FORM)

Character-by-character projection:
```
FORM=AAAA_SSS_AAAAAA_AAAAAA_SSS_AAAAAAA_AAAAAAAA...
```

**Key Insight**: Files with identical FORM patterns are **structurally equivalent** regardless of actual content. This is the basis for:
- Relational file classification
- Content-agnostic validation
- Geometric equivalence checking

---

## Step-by-Step: Creating ATOM.md Analysis

### Step 1: Navigate to Bootstrap Directory
```bash
cd /data/data/com.termux/files/home/tetragrammatron-os/tetragrammatron-os_bootstrap_A_then_B
```

### Step 2: Run Basic Genesis Analysis
```bash
./genesis.sh translate ../atom/ATOM.md
```

**Expected Output**:
```
HW=4E:00:81:F3:F3:E6:28:28
V8=230,945,944,27,4,1888,17,305
FORM=AAAA_SSS_AAAAAAAAA_AAAAAAA...
```

### Step 3: Run Enhanced Analysis with Tetragrammatron Symbols
```bash
GENESIS_P0_SYMBOL='α' GENESIS_P1_SYMBOL='Ω' GENESIS_WS_SYMBOL='∘' \
  ./genesis-enhanced.sh translate ../atom/ATOM.md
```

**Expected Output**:
```
HW=4E:00:81:F3:F3:E6:28:28
V8=230,945,944,27,4,1888,17,305
FORM=αααα∘ΩΩΩ∘αααααααα∘αααααααΩΩΩ...
# Legend: α=P₀(alphanumeric) Ω=P₁(structure) ∘=whitespace
```

### Step 4: Extract Just the Multiplexer (MUX8) Signature
```bash
./genesis-enhanced.sh mux8 ../atom/ATOM.md
```

**Output** (8-tuple metadata):
```
7494 1888 946 40 2 3776 17 305
```

This represents:
- 7494 chars total
- 1888 state changes
- 946 ignored (repeated non-alphanumeric)
- 40 max nesting depth
- 2 schema depth mismatches
- 3776 binding points
- 17 context switches (quotes)
- 305 column breaks (newlines)

---

## Using Genesis Probes in Tetragrammatron-OS Workflow

### A. Pre-Assembly Validation

Before assembling `.canasm` files to `.canb` bytecode:

```bash
# Check structural consistency
./genesis.sh translate src/canasm/seed/canasm1.canasm > /tmp/genesis_sig.txt

# Verify expected patterns (example: all .canasm files should have similar V8 profiles)
cat /tmp/genesis_sig.txt
```

### B. CANB Container Structural Fingerprinting

```bash
# Probe compiled bytecode
./genesis.sh translate /tmp/demo.canb

# Compare with source fingerprint
diff <(./genesis.sh translate src/demo.canasm | grep V8) \
     <(./genesis.sh translate /tmp/demo.canb | grep V8)
```

**Expectation**: V8 signatures should differ (CANASM is text, CANB is binary), but structural **relations** should be preserved.

### C. Document Integrity Checking

For documentation like ATOM.md:

```bash
# Generate baseline signature
./genesis-enhanced.sh translate ../atom/ATOM.md > atom_baseline.sig

# Later, verify integrity
./genesis-enhanced.sh translate ../atom/ATOM.md > atom_current.sig
diff atom_baseline.sig atom_current.sig
```

If HW tuple differs but V8 tuple matches → file moved/copied (metadata changed, structure intact)
If V8 tuple differs → **content was modified**

---

## Advanced: Genesis-Protocol Full Workflow

For complete literate programming bootstrap using `genesis.org`:

### Step 1: Install Emacs (if not available)
```bash
pkg install emacs  # Termux
# or: apt install emacs-nox  # Debian/Ubuntu
```

### Step 2: Run Genesis Bootstrap
```bash
cd /data/data/com.termux/files/home/tetragrammatron-os/genesis-protocol
./genesis.sh
```

This tangles `genesis.org` into:
- `.genesisignore` (blacklist for unknown-unknown files)
- `.genesisinclude` (whitelist for known-known files)
- `.genesisschema` (schema database)
- Layer 0-7 Scheme modules

### Step 3: Probe All Tracked Files
```bash
# Find all known-known files and probe them
find . -name "*.md" -o -name "*.c" -o -name "*.canasm" | while read f; do
  echo "=== $f ==="
  ../tetragrammatron-os_bootstrap_A_then_B/genesis-enhanced.sh translate "$f"
done > repository_signatures.txt
```

---

## Connection to ATOM Economy

The genesis-protocol analysis of ATOM.md demonstrates the **post-numeric philosophy**:

1. **ATOM.md describes economic relations** (OWNS, OWES, HOLDS)
2. **Genesis-protocol extracts structural relations** (alpha-runs, transitions, nesting)
3. **Both operate without numeric identity**:
   - ATOM: Asset identity = unique atom (not account balance)
   - Genesis: File identity = structural signature (not inode number)

### Example: ATOM Transaction as Genesis Probe

```bash
# Hypothetical: Probe a transaction record
echo "TRANSFER atom:alice:USD123 TO atom:bob AMOUNT 1" > tx.atom
./genesis-enhanced.sh translate tx.atom
```

**Output**:
```
HW=... (ephemeral, session-local)
V8=... (canonical structure)
FORM=AAAAAAAA∘αααααΩαααααΩααα...
```

The **V8 tuple is the canonical transaction signature**, not the byte content.

---

## Determinism and Reversibility

### Key Invariants

1. **Same file content → Same V8 tuple** (deterministic)
2. **Different V8 tuple → Different structure** (collision-resistant)
3. **Same FORM pattern → Structurally equivalent** (projection-invariant)

### Testing Determinism
```bash
# Test 1: Multiple runs produce identical V8
./genesis.sh translate ../atom/ATOM.md | grep V8 > run1.txt
./genesis.sh translate ../atom/ATOM.md | grep V8 > run2.txt
diff run1.txt run2.txt  # Should be empty

# Test 2: Copy should have same V8, different HW
cp ../atom/ATOM.md /tmp/ATOM_copy.md
./genesis.sh translate ../atom/ATOM.md | grep V8
./genesis.sh translate /tmp/ATOM_copy.md | grep V8  # Same V8
./genesis.sh translate ../atom/ATOM.md | grep HW
./genesis.sh translate /tmp/ATOM_copy.md | grep HW  # Different HW (inode/timestamps)
```

---

## Integration with CANASM/CANB Pipeline

### Workflow: Source → Assembly → Bytecode → Execution

```bash
# 1. Probe source structure
./genesis-enhanced.sh translate src/canasm/example.canasm > example.genesis

# 2. Assemble to bytecode
./bin/canasm0 src/canasm/example.canasm -o /tmp/example.canb

# 3. Probe bytecode structure
./genesis-enhanced.sh translate /tmp/example.canb > example_canb.genesis

# 4. Disassemble for verification
./bin/canasm0-disasm /tmp/example.canb > /tmp/example_disasm.canasm

# 5. Probe disassembled output
./genesis-enhanced.sh translate /tmp/example_disasm.canasm > example_disasm.genesis

# 6. Compare structural signatures
diff <(grep V8 example.genesis) <(grep V8 example_disasm.genesis)
# Expect: Identical V8 (round-trip preserves structure)
```

---

## Symbol Legend Reference

| Encoding | P₀ (Semantic) | P₁ (Structure) | Whitespace | Use Case                    |
|----------|---------------|----------------|------------|-----------------------------|
| Default  | `A`           | `S`            | `_`        | ASCII-safe environments     |
| Greek    | `α`           | `Ω`            | `∘`        | Tetragrammatron canonical   |
| Octonion | `e₀`          | `∑`            | `·`        | Mathematical analysis       |
| Alchemy  | `🜂`           | `🜃`            | `○`        | Esoteric/visual rendering   |
| Custom   | (any UTF-8)   | (any UTF-8)    | (any)      | Domain-specific encodings   |

**Critical Philosophy**: The **symbols don't matter**. Only the **alternation pattern** (shape) matters.

Files with:
```
A_S_A_A_S
α∘Ω∘α∘α∘Ω
🜂○🜃○🜂○🜂○🜃
```
Are **geometrically equivalent**.

---

## Practical Applications

### 1. Documentation Integrity
```bash
# Baseline all documentation
for doc in docs/*.md; do
  ./genesis-enhanced.sh translate "$doc" > "genesis_sigs/$(basename $doc).sig"
done

# Later: Verify no unintended changes
./genesis-enhanced.sh translate docs/ATOM.md | diff - genesis_sigs/ATOM.md.sig
```

### 2. Code Review Aid
```bash
# Before code review: Capture structural signature
./genesis.sh translate src/canvm/canvm_wasm.c > canvm_before.sig

# After changes: Compare
./genesis.sh translate src/canvm/canvm_wasm.c > canvm_after.sig
diff canvm_before.sig canvm_after.sig

# If V8[v5] (transitions) increases significantly → complexity added
# If V8[v6] (unique separators) changes → style/formatting changed
```

### 3. License/Template Detection
```bash
# All MIT licenses should have similar V8 profiles
find . -name "LICENSE" | while read lic; do
  echo -n "$lic: "
  ./genesis.sh translate "$lic" | grep V8
done
```

---

## Troubleshooting

### Issue: `genesis.sh: not found`
**Solution**: Ensure you're in the correct directory:
```bash
cd tetragrammatron-os_bootstrap_A_then_B
./genesis.sh translate ../atom/ATOM.md
```

### Issue: Unicode symbols not displaying
**Solution**: Ensure terminal supports UTF-8:
```bash
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
```

For Termux:
```bash
pkg install ncurses-utils
```

### Issue: Inconsistent V8 output
**Cause**: File was modified between runs.
**Verification**:
```bash
md5sum atom/ATOM.md  # Check file hash
```

### Issue: `awk` version differences
Genesis scripts use POSIX `awk`. If issues occur:
```bash
# Termux: Install gawk
pkg install gawk

# Then use gawk explicitly
sed -i 's/awk /gawk /g' genesis.sh
```

---

## Summary

**Creating ATOM.md with Tetragrammatron-OS** means:

1. **Writing relational documentation** (ATOM.md content) that describes post-numeric value
2. **Probing structural essence** with genesis-protocol tools
3. **Extracting geometric signatures** (8-tuples) that represent file identity
4. **Treating numbers as derived views**, not canonical truth

The file `atom/ATOM.md` both:
- **Describes** a relational economy where numbers are ephemeral
- **Embodies** that philosophy through genesis-protocol analysis

**Core Commands**:
```bash
# Basic probe
./genesis.sh translate ../atom/ATOM.md

# Enhanced with symbols
GENESIS_P0_SYMBOL='α' GENESIS_P1_SYMBOL='Ω' ./genesis-enhanced.sh translate ../atom/ATOM.md

# Just the 8-tuple
./genesis-enhanced.sh mux8 ../atom/ATOM.md
```

**Philosophy**: Relations precede numbers. Structure precedes syntax. Shape precedes symbols.

---

## Next Steps

1. **Probe all repository files**: Create a complete structural map
2. **Build CANASM→CANB pipeline**: Integrate genesis probes into build
3. **Implement ATOM validator**: Use Fano geometry + genesis signatures
4. **Create visual Fano projection**: SVG rendering of ATOM.md structure

See also:
- `CLAUDE.md` - Project philosophy and build instructions
- `docs/CONTRIBUTING.md` - Development principles
- `src/canb/spec/CANISA_CANB_BRIDGE.md` - Bytecode semantics
- `genesis-protocol/genesis.org` - Full literate programming bootstrap
