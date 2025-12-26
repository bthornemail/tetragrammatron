# Getting Started with Tetragrammatron

## Welcome

This guide will help you get started with the Tetragrammatron Project, from installation through your first programs.

## Prerequisites

### System Requirements

**Minimum**:
- POSIX-compatible shell (bash, sh, dash)
- C99 compiler (GCC, Clang, TCC)
- Make (GNU Make or compatible)
- 10 MB free disk space

**Recommended**:
- Python 3 (for web demos)
- Modern browser (for GLB/SVG visualization)
- Git (for version control, optional)

### Supported Platforms

✅ **Linux** (x86_64, ARM, RISC-V)
✅ **Android/Termux** (ARM, ARM64)
✅ **macOS** (x86_64, ARM64)
✅ **BSD** (FreeBSD, OpenBSD)
🔨 **ESP32** (planned - bare metal)
✅ **Browser** (WebAssembly)

## Installation

### Step 1: Clone or Download

**Option A: Git**
```bash
git clone https://github.com/yourorg/tetragrammatron.git
cd tetragrammatron
```

**Option B: Download**
```bash
# Download release tarball
wget https://github.com/yourorg/tetragrammatron/releases/latest/tetragrammatron.tar.gz
tar xzf tetragrammatron.tar.gz
cd tetragrammatron
```

### Step 2: Build the Toolchain

```bash
cd tetragrammatron-os/
make
```

**Expected output**:
```
cc -std=c99 -O2 -Wall -Wextra -Werror \
  -o bin/canasm0 src/canasm/seed/canasm0.c

cc -std=c99 -O2 -Wall -Wextra -Werror \
  -o bin/canasm0-disasm src/canasm/seed/canasm0_disasm.c

Build complete!
```

**Verify**:
```bash
ls -lh bin/
# Should show:
# canasm0        (11K) - Assembler
# canasm0-disasm (8.8K) - Disassembler
```

### Step 3: Test the Installation

```bash
# Assemble test program
./bin/canasm0 src/canasm/seed/canasm1.canasm -o ~/test.canb

# Disassemble to verify
./bin/canasm0-disasm ~/test.canb
```

**Expected output**:
```
; Disassembled from /home/user/test.canb (CANB v1)

@atoms
  start
  state
  accept
@end

@code
  ATOM start
  ATOM state
  EDGE
  ATOM state
  ATOM accept
  EDGE
  HALT
@end
```

If you see this output, **installation succeeded!** ✅

## Your First CANB Program

### Hello, Relations!

Create a file `hello.canasm`:

```canasm
// My first CANB program
// Creates two atoms and an edge: start → accept

PUSH ATOM:start
PUSH ATOM:accept
EDGE_ADD
HALT
```

### Assemble It

```bash
cd ~/tetragrammatron/tetragrammatron-os/
./bin/canasm0 ~/hello.canasm -o ~/hello.canb
```

**Output**: `~/hello.canb` (binary file, ~20 bytes)

### Inspect It

```bash
# View disassembly
./bin/canasm0-disasm ~/hello.canb

# View hex dump
hexdump -C ~/hello.canb | head
```

**Disassembly output**:
```
@atoms
  start
  accept
@end

@code
  ATOM start
  ATOM accept
  EDGE
  HALT
@end
```

### Validate It

```bash
cd ~/tetragrammatron/
./integration/validators/canb-fano-validator.sh ~/hello.canb
```

**Output**:
```
VALID: {e6}
```

Your program is **Fano-valid**! The atom `start` maps to e₆, and a single point trivially satisfies closure.

### Diffuse It (Structural Fingerprint)

```bash
cd ~/tetragrammatron/revelation-observer/
./bin/genesis-diffuse.sh ~/hello.canb
```

**Output**:
```
a3f7c8e1b4d6f9a2c5e8b1d4f7a0c3e6b9d2f5a8c1e4b7d0f3a6c9e2b5d8f1a4
```

This is your program's **structural basis** - a deterministic fingerprint generated through 8-pass regex diffusion.

**What this means**:
- Same structure → same hash
- Different structure → different hash
- No semantic interpretation involved

**Try verbose mode**:
```bash
./bin/genesis-diffuse.sh --verbose ~/hello.canb
```

You'll see all 8 passes: Identity, Alphanumeric collapse, Non-alphanumeric isolation, Boundary normalization, Repetition compression, Symmetry check, Context adjacency, and Closure.

## Understanding CANB Programs

### Program Structure

Every CANB program has:

1. **Atom Table**: Symbolic names (strings)
2. **Code**: Instructions that manipulate atoms

### Instructions

| Instruction | Opcode | Arguments | Description |
|------------|--------|-----------|-------------|
| `PUSH ATOM:name` | 0x01 | atom_id | Push atom onto stack |
| `EDGE_ADD` | 0x02 | - | Pop two atoms, add edge |
| `PROJ_FANO` | 0x03 | - | Project to Fano plane |
| `HALT` | 0xFF | - | Stop execution |

### Stack Model

```
Initial:  []
PUSH a:   [a]
PUSH b:   [a, b]
EDGE_ADD: []          (creates edge a→b)
```

### Example: Three-Node Graph

```canasm
// Create: start → state → accept

PUSH ATOM:start
PUSH ATOM:state
EDGE_ADD              // Edge: start → state

PUSH ATOM:state
PUSH ATOM:accept
EDGE_ADD              // Edge: state → accept

HALT
```

## Working with Genesis Protocol

### Generate Fingerprints

```bash
cd ~/tetragrammatron/genesis-protocol/

# Fingerprint your program
./genesis.sh translate ~/hello.canb
```

**Output**:
```
HW=0A:34:51:4B:4B:D1:0D:0D
V8=[15,167,249,22,90,107,15,15]
FORM=AAAA_ASSSSSS_A_AAAA_
```

**Meaning**:
- **HW**: Hardware fingerprint (8 bytes)
- **V8**: Topology vector (8 integers from stat)
- **FORM**: P₀/P₁ alternation pattern

### Custom Symbols

```bash
# Use Greek symbols
GENESIS_P0_SYMBOL=α GENESIS_P1_SYMBOL=Ω \
  ./genesis-enhanced.sh translate ~/hello.canb
```

**Output**:
```
FORM=αααα∘αΩΩΩΩΩΩ∘α∘αααα∘
```

Same structure, different symbols!

### Generate Atoms Database

```bash
cd ~/tetragrammatron/

# Fingerprint entire codebase
./genesis-protocol/genesis.sh probe tetragrammatron-os/ atoms.jsonl
```

**Output**: `atoms.jsonl` with entries like:
```json
{"t":"2025-12-25T...","path":"bin/canasm0","id":"BINCANASM0","stat":{...},"hw":"...","v":[...]}
```

## Structural Diffusion (Revelation Observer)

The **Revelation Observer** provides observation without collapse - viewing structural emergence through 8-pass regex diffusion without semantic interpretation.

### Philosophy

> "The Observer doesn't respond. It reflects."

**Key principles**:
- **Observation without collapse** - Measurement reveals state without destroying it
- **Regex-only constraint** - No parsing, no ASTs, no semantic interpretation
- **Diffusion not transformation** - Reveals what's already there
- **Deterministic basis** - Same structure → same fingerprint

### Basic Usage

**Get structural basis** for any file:

```bash
cd ~/tetragrammatron/revelation-observer/

# Diffuse a file
./bin/genesis-diffuse.sh ~/hello.canb
```

**Output**: SHA256 hash (structural fingerprint)

**Verbose mode** to see all 8 passes:

```bash
./bin/genesis-diffuse.sh --verbose ~/hello.canb
```

**Output**:
```
=== Pass 0: Identity (anchor) ===
Length: 157 bytes

=== Pass 1: Alphanumeric collapse (software projection) ===
Sample: CANB start accept
Length: 155 bytes

=== Pass 2: Non-alphanumeric isolation (syntax projection) ===
Sample: @ @ @
Length: 157 bytes

=== Pass 3: Boundary normalization ===
Length: 68 bytes

=== Pass 4: Repetition compression ===
Length: 65 bytes

=== Pass 5: Symmetry check (sort + uniq) ===
Unique lines: 12

=== Pass 6: Context adjacency (sliding window) ===
Length: 71 bytes

=== Pass 7: Closure (basis promotion) ===
Basis hash: a3f7c8e1b4d6f9a2c5e8b1d4f7a0c3e6b9d2f5a8c1e4b7d0f3a6c9e2b5d8f1a4

=== Diffusion Complete ===
```

### The 8-Pass Model

Each pass projects the input through a specific regex operation:

1. **Pass 0: Identity** - Anchor (preserve original)
2. **Pass 1: Alphanumeric Collapse** - Software projection (semantic mass)
3. **Pass 2: Non-Alphanumeric Isolation** - Syntax projection (structure)
4. **Pass 3: Boundary Normalization** - Collapse whitespace
5. **Pass 4: Repetition Compression** - Remove redundant repetition
6. **Pass 5: Symmetry Check** - Sort + unique (order-independent patterns)
7. **Pass 6: Context Adjacency** - Sliding window (local relationships)
8. **Pass 7: Closure** - SHA256 hash (irreversible projection to basis)

**Properties**:
- Each pass is **idempotent** (running twice = running once)
- Passes are **order-sensitive** (Pass N depends on Pass N-1)
- Result is **deterministic** (same input → same output)

### Compare Files Structurally

```bash
# Get basis for two files
basis_a=$(./bin/genesis-diffuse.sh ~/hello.canb)
basis_b=$(./bin/genesis-diffuse.sh ~/hello.canasm)

# Compare
if [ "$basis_a" = "$basis_b" ]; then
  echo "Structurally identical"
else
  echo "Structurally different"
  echo "  File A: $basis_a"
  echo "  File B: $basis_b"
fi
```

### Find Structurally Similar Files

```bash
# Diffuse all CANB files
for f in *.canb; do
  basis=$(./bin/genesis-diffuse.sh "$f")
  echo "$basis $f"
done | sort

# Files with same basis are structurally equivalent
```

### Genesis vs Revelation

**Complementary roles**:

| Aspect | Genesis | Revelation |
|--------|---------|------------|
| **Focus** | Hardware + POSIX metadata | Pure structure |
| **Output** | HW (8 bytes) + V8 (8 ints) + FORM | SHA256 basis hash |
| **Uses** | Provenance, distributed sync | Structural comparison |

**Combined usage**:
```bash
# Genesis fingerprint (provenance)
hw_v8=$(../genesis-protocol/genesis.sh translate ~/hello.canb | grep "HW=")

# Revelation basis (structure)
basis=$(./bin/genesis-diffuse.sh ~/hello.canb)

echo "Genesis (provenance): $hw_v8"
echo "Revelation (structure): $basis"
```

**Learn more**: See [Revelation Observer Guide](revelation-observer.md) for comprehensive documentation.

## Visualization

### 2D Fano Plane (SVG)

```bash
cd tetragrammatron-os/web/demo/

# Start web server
python3 -m http.server 8000

# Open browser to http://localhost:8000
```

You'll see an interactive Fano plane visualization showing the 7 points and 7 lines.

### 3D Merkaba (GLB)

```bash
# Open browser to http://localhost:8000/glb_demo.html
```

Click "Export Merkaba GLB" to download a 3D model of the dual tetrahedron (Merkaba) representing the Fano plane.

**Import into**:
- Blender (File → Import → glTF)
- Three.js viewer
- Unity/Unreal Engine
- Any glTF-compatible tool

## Validation

### Fano-Gate Validator

The validator checks if your program's atoms satisfy **octonionic closure** (Fano plane constraints).

**Valid program** (complete Fano line):
```canasm
// Line 0: {accept, alphabet, left} = {e₁, e₂, e₃}
PUSH ATOM:accept
PUSH ATOM:alphabet
PUSH ATOM:left
EDGE_ADD
EDGE_ADD
HALT
```

```bash
./bin/canasm0 valid.canasm -o valid.canb
./integration/validators/canb-fano-validator.sh valid.canb
# Output: VALID: {e1,e2,e3}
```

**Invalid program** (incomplete Fano line):
```canasm
// Only 2/3 of line 0 (missing left)
PUSH ATOM:accept
PUSH ATOM:alphabet
EDGE_ADD
HALT
```

```bash
./bin/canasm0 invalid.canasm -o invalid.canb
./integration/validators/canb-fano-validator.sh invalid.canb
# Output: INVALID: Atom set violates Fano closure
```

### Canonical Atoms

For Fano validation, use these canonical names:

| Atom | Octonion Unit | Fano Point |
|------|---------------|------------|
| `accept` | e₁ | 0 |
| `alphabet` | e₂ | 1 |
| `left` | e₃ | 2 |
| `right` | e₄ | 3 |
| `delta` | e₅ | 4 |
| `start` | e₆ | 5 |
| `state` | e₇ | 6 |
| `reject` | e₀ | (identity) |

**Non-canonical atoms** (like `hello`, `world`) are allowed but don't participate in Fano validation.

## Common Tasks

### Task 1: Assemble and Test

```bash
# Write program
cat > myprogram.canasm << 'EOF'
PUSH ATOM:start
PUSH ATOM:accept
EDGE_ADD
HALT
EOF

# Assemble
./tetragrammatron-os/bin/canasm0 myprogram.canasm -o myprogram.canb

# Verify
./tetragrammatron-os/bin/canasm0-disasm myprogram.canb

# Validate
./integration/validators/canb-fano-validator.sh myprogram.canb
```

### Task 2: Genesis Fingerprint

```bash
# Single file
./genesis-protocol/genesis.sh translate myprogram.canb

# Whole directory
./genesis-protocol/genesis.sh probe tetragrammatron-os/ atoms.jsonl
```

### Task 3: Visualize

```bash
cd tetragrammatron-os/web/demo/
python3 -m http.server 8000
# Open http://localhost:8000
```

### Task 4: Round-trip Test

```bash
# Assemble
./bin/canasm0 input.canasm -o a.canb

# Disassemble
./bin/canasm0-disasm a.canb > output.canasm

# Reassemble
./bin/canasm0 output.canasm -o b.canb

# Compare (should be identical)
cmp a.canb b.canb && echo "PASS" || echo "FAIL"
```

### Task 5: Structural Diffusion

```bash
# Get structural basis
./revelation-observer/bin/genesis-diffuse.sh myprogram.canb

# Compare two files structurally
basis_a=$(./revelation-observer/bin/genesis-diffuse.sh file_a.c)
basis_b=$(./revelation-observer/bin/genesis-diffuse.sh file_b.c)
[ "$basis_a" = "$basis_b" ] && echo "Same structure" || echo "Different structure"

# Verbose inspection
./revelation-observer/bin/genesis-diffuse.sh --verbose myprogram.canb
```

## Troubleshooting

### Error: "Command not found: canasm0"

**Cause**: Binary not in PATH or not built

**Solution**:
```bash
cd tetragrammatron-os/
make
./bin/canasm0  # Use relative path
```

### Error: "CANASM0 error: failed to open input"

**Cause**: File doesn't exist or wrong path

**Solution**:
```bash
# Use absolute path
./bin/canasm0 /full/path/to/program.canasm -o output.canb

# Or check file exists
ls -l program.canasm
```

### Error: "bad magic (expected CANB)"

**Cause**: Input file is not CANB format

**Solution**:
```bash
# Verify file is CANB
hexdump -C file.canb | head -1
# Should start with: 43 41 4e 42  (ASCII: CANB)

# If not, assemble first
./bin/canasm0 source.canasm -o file.canb
```

### Error: "INVALID: Atom set violates Fano closure"

**Cause**: Program uses 2 out of 3 atoms from a Fano line

**Solution**: Add the third atom or use different atoms
```bash
# Check which line is incomplete
./integration/validators/canb-fano-validator.sh program.canb
# Output shows which line violated

# Fix: complete the line or remove atoms
```

## Next Steps

### Learn More

- **[Building Programs](building-programs.md)** - Write more complex CANB programs
- **[Revelation Observer Guide](revelation-observer.md)** - Master structural diffusion
- **[Genesis Protocol Guide](genesis-guide.md)** - Master fingerprinting and sync
- **[Validation Guide](validation-guide.md)** - Understand Fano-gate in depth

### Explore Examples

```bash
cd integration/examples/
# (Coming soon)
```

### Read Architecture

- **[Architecture Overview](../architecture/overview.md)** - Understand the system design
- **[Projection Model](../architecture/projection-model.md)** - Ball/Sphere/Projection theory

### Contribute

- **[CONTRIBUTING.md](../CONTRIBUTING.md)** - How to contribute
- Report issues on GitHub
- Join the community

## Quick Reference

### Commands

```bash
# Assemble
./tetragrammatron-os/bin/canasm0 input.canasm -o output.canb

# Disassemble
./tetragrammatron-os/bin/canasm0-disasm input.canb

# Validate
./integration/validators/canb-fano-validator.sh program.canb

# Fingerprint
./genesis-protocol/genesis.sh translate file

# Probe directory
./genesis-protocol/genesis.sh probe dir/ atoms.jsonl

# Structural diffusion
./revelation-observer/bin/genesis-diffuse.sh file

# Structural diffusion (verbose)
./revelation-observer/bin/genesis-diffuse.sh --verbose file

# Web demo
cd tetragrammatron-os/web/demo/ && python3 -m http.server 8000
```

### File Extensions

- `.canasm` - CANASM assembly source (text)
- `.canb` - CANB bytecode container (binary)
- `.jsonl` - Genesis atoms database (JSON Lines)
- `.svg` - 2D Fano plane visualization
- `.glb` - 3D Merkaba model (glTF binary)

### Environment Variables

```bash
# Genesis symbol customization
export GENESIS_P0_SYMBOL=α      # Alphanumeric symbol
export GENESIS_P1_SYMBOL=Ω      # Structure symbol
export GENESIS_WS_SYMBOL=∘      # Whitespace symbol
```

## Summary

You've learned to:

✅ Install and build Tetragrammatron
✅ Write and assemble CANB programs
✅ Validate programs with Fano-gate
✅ Generate Genesis fingerprints
✅ Diffuse files to structural basis (Revelation Observer)
✅ Visualize Fano planes (2D/3D)

**Next**: Dive deeper into [Building Programs](building-programs.md) to create more sophisticated CANB applications!

---

**Need help?** Check the [FAQ](faq.md) or report issues on GitHub.
