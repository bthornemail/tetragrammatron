# System Design

## Component Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    Tetragrammatron Project                  │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────────┐  ┌──────────────────┐  ┌───────────┐ │
│  │ Tetragrammatron  │  │     Genesis      │  │Integration│ │
│  │       OS         │  │    Protocol      │  │           │ │
│  └──────────────────┘  └──────────────────┘  └───────────┘ │
│         │                     │                    │        │
│         ├─ CANASM             ├─ genesis.sh        ├─ Validators│
│         ├─ CANB               ├─ genesis-enhanced  ├─ Examples  │
│         ├─ CANVM              ├─ TPPM              └─ Control   │
│         ├─ Projections        └─ Fingerprinting               │
│         └─ WebAssembly                                        │
│                                                               │
└───────────────────────────────────────────────────────────────┘
```

## Tetragrammatron-OS Design

### CANASM (Assembler)

**Purpose**: Transform human-readable assembly into CANB bytecode

**Input Format**:
```canasm
// Comment
PUSH ATOM:name    // Push atom onto stack
EDGE_ADD          // Pop two atoms, add edge
PROJ_FANO         // Project to Fano plane
HALT              // Stop execution
```

**Output Format** (CANB v1):
```
Offset  Field           Type        Description
0-3     Magic           [4]byte     "CANB"
4       Version         uint8       1
5+      AtomCount       ULEB128     Number of atoms
...     Atoms           []Atom      Atom table
...     CodeLen         ULEB128     Code length in bytes
...     Code            []byte      Bytecode
```

**Implementation** (`src/canasm/seed/canasm0.c`):
- Lexer: Tokenize lines (space-separated)
- Parser: Recognize instructions (PUSH, EDGE_ADD, etc.)
- Atom table: Intern strings (deduplicate)
- Code generation: Emit ULEB128-encoded opcodes
- Output: Write CANB container

**Key Algorithms**:

*Atom Interning*:
```c
typedef struct {
  char** items;
  size_t len, cap;
} StrVec;

int sv_intern(StrVec* v, const char* s) {
  int idx = sv_find(v, s);  // O(n) search
  if (idx >= 0) return idx;
  sv_push(v, s);             // Add if not found
  return (int)(v->len - 1);
}
```

*ULEB128 Encoding*:
```c
void bb_put_uleb(ByteBuf* bb, uint32_t v) {
  while (1) {
    uint8_t byte = (uint8_t)(v & 0x7F);
    v >>= 7;
    if (v) byte |= 0x80;      // Continuation bit
    bb_put(bb, byte);
    if (!v) break;
  }
}
```

**Determinism**:
- Atoms sorted alphabetically before encoding (future enhancement)
- Current: First-use order (deterministic given fixed input)
- ULEB128 encoding is canonical (shortest representation)

### CANB (Binary Container)

**Design Goals**:
- Platform-independent (no alignment assumptions)
- Compact (variable-length encoding)
- Streamable (can parse without seeking)
- Extensible (version field)

**Format Specification**:

```
CANB := Magic Version AtomTable Code

Magic := 0x43 0x41 0x4E 0x42  // "CANB"

Version := 0x01

AtomTable := ULEB128(count) Atom*

Atom := ULEB128(len) UTF8(len)

Code := ULEB128(len) Opcode*

Opcode := PUSH_ATOM | EDGE_ADD | PROJ_FANO | HALT

PUSH_ATOM := 0x01 ULEB128(atom_id)
EDGE_ADD  := 0x02
PROJ_FANO := 0x03
HALT      := 0xFF
```

**Example Encoding**:
```
Input:
  PUSH ATOM:accept
  PUSH ATOM:alphabet
  EDGE_ADD
  HALT

Hex dump:
  43 41 4E 42        // Magic "CANB"
  01                 // Version 1
  02                 // Atom count = 2
  06 61 63 63 65 70 74  // Atom 0: "accept" (len=6)
  08 61 6C 70 68 61 62 65 74  // Atom 1: "alphabet" (len=8)
  06                 // Code length = 6
  01 00              // PUSH_ATOM 0
  01 01              // PUSH_ATOM 1
  02                 // EDGE_ADD
  FF                 // HALT
```

**Size Analysis**:
- Minimal program: ~15 bytes (magic + version + empty atoms + HALT)
- Typical program: ~25-50 bytes
- Large program: Linear in atom count + code size

### CANVM (Execution Engine)

**Design**: Stack-based VM with graph store

**State**:
```c
typedef struct {
  Atom stack[256];      // Atom stack (256 max depth)
  int sp;               // Stack pointer

  Graph graph;          // Relation store

  uint8_t* code;        // Bytecode
  size_t pc;            // Program counter

  char** atoms;         // Atom table (from CANB)
  size_t atom_count;
} VM;
```

**Execution Loop**:
```c
void vm_run(VM* vm) {
  while (1) {
    uint8_t op = vm->code[vm->pc++];

    switch (op) {
      case 0x01: { // PUSH_ATOM
        uint32_t id = read_uleb(&vm->pc);
        vm_push(vm, vm->atoms[id]);
        break;
      }

      case 0x02: { // EDGE_ADD
        Atom b = vm_pop(vm);
        Atom a = vm_pop(vm);
        graph_add_edge(&vm->graph, a, b);
        break;
      }

      case 0x03: { // PROJ_FANO
        Atom a = vm_peek(vm);
        fano_project(&vm->graph, a);
        break;
      }

      case 0xFF: // HALT
        return;
    }
  }
}
```

**Graph Representation**:
```c
typedef struct {
  Atom src, dst;
} Edge;

typedef struct {
  Edge* edges;
  size_t len, cap;
} Graph;
```

**Operations**:
- `graph_add_edge(g, a, b)` - Add directed edge a→b
- `graph_find_edges(g, a)` - Find all edges from a
- `graph_has_path(g, a, b)` - Check if path exists a⇝b

**Projection Interface**:
```c
typedef void (*ProjectionFn)(Graph* g, void* ctx);

// SVG projection
void proj_svg_fano(Graph* g, FILE* out);

// GLB projection (3D Merkaba)
void proj_glb_merkaba(Graph* g, FILE* out);

// POSIX projection (filesystem view)
void proj_posix(Graph* g, PosixCtx* ctx);
```

### WebAssembly VM

**File**: `src/canb/wasm/canvm_wasm.c`

**Purpose**: Enable CANB execution in browsers

**Build**:
```bash
clang --target=wasm32 -nostdlib -Wl,--no-entry \
  -Wl,--export-all -O2 \
  -o canvm.wasm canvm_wasm.c
```

**Exported Functions**:
```c
// Initialize VM with CANB bytecode
void* canvm_init(uint8_t* canb, size_t len);

// Execute one instruction
int canvm_step(void* vm);

// Get projection output
uint8_t* canvm_get_output(void* vm, size_t* len);
```

**JavaScript Integration**:
```javascript
const { instance } = await WebAssembly.instantiate(wasmBytes);

// Load CANB file
const canb = await fetch('program.canb').then(r => r.arrayBuffer());
const vm = instance.exports.canvm_init(new Uint8Array(canb));

// Execute
while (instance.exports.canvm_step(vm) !== 0) {
  // Running...
}

// Get results
const output = instance.exports.canvm_get_output(vm);
```

## Genesis Protocol Design

### Two-Primitive Projection Model (TPPM)

**Core Algorithm**:
```bash
software_form_probe() {
  file="$1"

  awk '
    {
      line = $0 "\n";
      for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1);

        if (c ~ /[[:alnum:]]/) {
          printf "A";       # P₀: Semantic mass
        } else if (c ~ /[[:space:]]/) {
          printf "_";       # Whitespace
        } else {
          printf "S";       # P₁: Structure
        }
      }
    }
  ' "$file"
}
```

**Output**: FORM skeleton (alternation pattern)

**Example**:
```c
int main() {
  return 0;
}
```

→ FORM: `AAASAAAASSSS_SS_SSAAAASSSS_S_S`

**Properties**:
- P₀/P₁ partition is complete (every byte classified)
- Whitespace preserved as separate class
- Pattern is symbol-agnostic

### 8-Dimensional Fingerprinting

**Components**:

1. **HW (Hardware)** - 8 bytes
   ```bash
   # Platform fingerprint
   uname -m | sha256sum | head -c 16
   ```

   Example: `0A:34:51:4B:4B:D1:0D:0D`

2. **V8 (Topology)** - 8 integers
   ```bash
   # Structural vector
   stat --format="%d %i %a %u %g %s %X %Y" file
   ```

   Example: `[207,89,90,13,20,178,28,12]`

3. **FORM** - Skeleton string
   ```
   AAAASSSAAAAASAAAASSSSSSSS_
   ```

**Combined Fingerprint**:
```json
{
  "hw": "0A:34:51:4B:4B:D1:0D:0D",
  "v": [207,89,90,13,20,178,28,12],
  "form": "AAAASSSAAAAASAAAASSSSSSSS_"
}
```

**Comparison**:
- **HW match**: Same hardware platform
- **V8 match**: Same file structure (size, timestamps, etc.)
- **FORM match**: Same content structure (P₀/P₁ alternation)

### Customizable Symbols

**Enhancement**: `genesis-enhanced.sh`

**Environment Variables**:
```bash
GENESIS_P0_SYMBOL=α      # Default: "A"
GENESIS_P1_SYMBOL=Ω      # Default: "S"
GENESIS_WS_SYMBOL=∘      # Default: "_"
```

**Implementation**:
```bash
software_form_probe() {
  file="$1"
  p0="${GENESIS_P0_SYMBOL:-A}"
  p1="${GENESIS_P1_SYMBOL:-S}"
  ws="${GENESIS_WS_SYMBOL:-_}"

  awk -v P0="$p0" -v P1="$p1" -v WS="$ws" '
    {
      # ... same logic, output P0/P1/WS variables
    }
  ' "$file"
}
```

**Structural Equivalence**:
```bash
# All produce same pattern, different symbols
FORM_ASCII="AAAA SSS AAAAA"
FORM_GREEK="αααα ΩΩΩ ααααα"
FORM_OCTO="e0e0e0e0 ΣΣΣ e0e0e0e0e0"

# Extract pattern (normalize to X/Y)
sed 's/A/X/g; s/S/Y/g' → "XXXX YYY XXXXX"
sed 's/α/X/g; s/Ω/Y/g' → "XXXX YYY XXXXX"
sed 's/e0/X/g; s/Σ/Y/g' → "XXXX YYY XXXXX"
```

All are structurally identical!

## Integration Layer Design

### Fano-Gate Validator

**Purpose**: Enforce octonionic closure on CANB programs

**Algorithm**:
```bash
validate_fano_closure() {
  atoms="$1"  # Space-separated atom names

  # Map atoms to Fano points (0-6)
  points=""
  for atom in $atoms; do
    case "$atom" in
      accept)   points="$points 0" ;;  # e₁
      alphabet) points="$points 1" ;;  # e₂
      left)     points="$points 2" ;;  # e₃
      right)    points="$points 3" ;;  # e₄
      delta)    points="$points 4" ;;  # e₅
      start)    points="$points 5" ;;  # e₆
      state)    points="$points 6" ;;  # e₇
    esac
  done

  # Check each Fano line
  for line in "0,1,2" "0,3,4" "0,5,6" "1,3,6" "1,4,5" "2,3,5" "2,4,6"; do
    count_in_set "$points" "$line"

    # Violation: exactly 2/3 points from a line
    if [ $count -eq 2 ]; then
      echo "INVALID: Line $line incomplete"
      return 1
    fi
  done

  echo "VALID"
  return 0
}
```

**Time Complexity**: O(7 × |atoms|) = O(|atoms|)

**Space Complexity**: O(|atoms|)

### Genesis Control Files

**`.genesis`** - Functor routing table:
```
# UU: Both unchanged (hash match)
UU: keep_local

# KU: Known locally, unknown remotely
KU: offer_to_peer

# UK: Unknown locally, known remotely
UK: fetch_from_peer

# KK: Known both, but different
KK: reconcile_conflict
```

**`.genesisignore`** - Exclusion patterns:
```
# Build artifacts
bin/
*.o
*.wasm

# Editor files
.vscode/
*.swp
```

**`.genesisinclude`** - Inclusion patterns:
```
# Source files
*.c
*.h
*.canasm

# Documentation
*.md
*.org
```

**Integration**:
```bash
genesis.sh probe tetragrammatron-os/ atoms.jsonl \
  --ignore-file .genesisignore \
  --include-file .genesisinclude
```

## Data Flow Diagrams

### Assembly Flow

```
┌──────────┐
│ .canasm  │
│  source  │
└────┬─────┘
     │
     ▼
┌──────────┐
│  CANASM  │  Parse & assemble
│ canasm0  │
└────┬─────┘
     │
     ▼
┌──────────┐
│  .canb   │  Binary container
│ bytecode │
└────┬─────┘
     │
     ▼
┌──────────┐
│  Fano    │  Validate closure
│  Gate    │
└────┬─────┘
     │
     ├─ VALID ──────────┐
     │                  ▼
     │            ┌──────────┐
     │            │  CANVM   │  Execute
     │            │   VM     │
     │            └────┬─────┘
     │                 │
     │                 ▼
     │            ┌──────────┐
     │            │Projection│  Output
     │            └──────────┘
     │
     └─ INVALID ─► REJECT
```

### Genesis Fingerprinting Flow

```
┌──────────┐
│   File   │
│  System  │
└────┬─────┘
     │
     ▼
┌──────────┐
│ Genesis  │  Probe files
│  probe   │
└────┬─────┘
     │
     ├─ For each file:
     │  │
     │  ├─► HW fingerprint (hardware)
     │  ├─► V8 vector (stat)
     │  └─► FORM (P₀/P₁ alternation)
     │
     ▼
┌──────────┐
│  Atoms   │  JSONL database
│   .jsonl │
└────┬─────┘
     │
     ▼
┌──────────┐
│  Peer    │  Exchange & reconcile
│  Sync    │
└──────────┘
```

## Concurrency Model

**Current**: Single-threaded, deterministic

**Future**: Multi-threaded graph queries

### Thread Safety Requirements

**Read-Only Operations** (safe to parallelize):
- Graph queries (find_edges, has_path)
- Projections (SVG, GLB generation)
- Validation (Fano-gate checking)

**Write Operations** (require synchronization):
- Graph mutations (add_edge, remove_edge)
- VM state updates (stack, PC)

**Proposed Model**:
```
Thread 1: VM execution (exclusive graph write access)
Thread 2-N: Projection workers (read-only graph access)

Synchronization: RWLock on graph
- VM holds write lock during EDGE_ADD
- Projections hold read lock during queries
```

## Error Handling

### Assembly Errors

```c
// canasm0.c error handling
void die(const char* msg) {
  fprintf(stderr, "CANASM0 error: %s\n", msg);
  exit(1);
}

// Usage
if (atom_id >= atom_count) {
  die("ATOM id out of range");
}
```

**Error Categories**:
- Syntax errors (unknown instruction)
- Semantic errors (atom out of range)
- I/O errors (can't open file)

### Runtime Errors

```c
// VM error handling
typedef enum {
  VM_OK = 0,
  VM_STACK_OVERFLOW,
  VM_STACK_UNDERFLOW,
  VM_INVALID_OPCODE,
  VM_OUT_OF_MEMORY
} VMError;

VMError vm_run(VM* vm) {
  // Check bounds before stack operations
  if (vm->sp >= 256) return VM_STACK_OVERFLOW;
  if (vm->sp < 0) return VM_STACK_UNDERFLOW;

  // Validate opcodes
  if (op >= 0x10 && op != 0xFF) {
    return VM_INVALID_OPCODE;
  }

  return VM_OK;
}
```

### Validation Errors

```bash
# Fano-gate validator
if ! check_fano_closure "$points"; then
  echo "INVALID: Atom set violates Fano closure" >&2
  echo "         Atoms: $atoms" >&2
  echo "         Points: $points" >&2
  exit 1
fi
```

**Error Output**:
```
VIOLATED: Line [0,1,2] has 2/3 points in set
          Points (e1,e2,e3): exactly 2/3 present (missing closure)
INVALID: Atom set violates Fano closure
         Atoms: accept alphabet
         Points: 0 1
         Residue: {e1,e2}
```

## Testing Strategy

### Unit Tests

**CANASM**:
- Parse valid instructions
- Reject invalid syntax
- Generate correct bytecode
- Handle edge cases (empty file, large atoms)

**CANVM**:
- Execute each opcode correctly
- Handle stack overflow/underflow
- Validate graph operations
- Test projection output

**Genesis**:
- P₀/P₁ classification correctness
- HW fingerprint consistency
- V8 vector accuracy
- Symbol substitution equivalence

### Integration Tests

**Round-trip**:
```bash
# Assemble → Disassemble → Reassemble
canasm0 input.canasm -o a.canb
canasm0-disasm a.canb > output.canasm
canasm0 output.canasm -o b.canb
cmp a.canb b.canb  # Must be identical
```

**Validation chain**:
```bash
# Valid program should pass
canasm0 valid.canasm -o valid.canb
canb-fano-validator.sh valid.canb  # Exit 0

# Invalid program should fail
canasm0 invalid.canasm -o invalid.canb
canb-fano-validator.sh invalid.canb  # Exit 1
```

### Property-Based Tests

**Determinism**:
```bash
# Same input → same output (always)
for i in 1 2 3 4 5; do
  canasm0 program.canasm -o run$i.canb
done
md5sum run*.canb  # All hashes must match
```

**Idempotence**:
```bash
# proj(x) = proj(proj(x))
proj_svg graph.canb > out1.svg
proj_svg out1.svg > out2.svg  # If applicable
diff out1.svg out2.svg        # Should match
```

## Performance Optimization

### Current Focus

**Correctness over speed**:
- Deterministic behavior
- Mathematical validation
- Platform portability

### Future Optimizations

**CANASM**:
- Hash table for atom interning (O(1) vs O(n))
- Preallocation for bytecode buffer
- Parallel file processing

**CANVM**:
- JIT compilation for hot loops
- Graph query caching
- SIMD for projection calculations

**Genesis**:
- Incremental fingerprinting (skip unchanged files)
- Parallel file processing
- mmap for large files

### Profiling Points

**Assembly**:
- Atom interning time
- ULEB128 encoding time
- File I/O time

**Execution**:
- Opcode dispatch time
- Graph operation time
- Projection generation time

**Validation**:
- Fano line checking time
- Atom extraction time

---

**See also**:
- [Architecture Overview](overview.md)
- [Data Structures](data-structures.md)
- [Performance Benchmarks](../reference/benchmarks.md)
