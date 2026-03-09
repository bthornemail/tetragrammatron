# Projection Model: Ball, Sphere, and Projection

## Introduction

The **Ball/Sphere/Projection** model is the philosophical foundation of Tetragrammatron-OS. It separates **physical constraints** (Ball), **semantic invariants** (Sphere), and **compatibility interfaces** (Projection) into distinct conceptual layers.

## The Three-Layer Model

### Layer 1: Ball (Hardware)

**Definition**: The physical, observable, platform-specific reality

**Properties**:
- Non-canonical (not stable truth)
- Platform-dependent
- Observable but discardable
- Changes with hardware/environment

**Examples**:
- Memory addresses: `0x7fff5fbff8a0` (changes every run)
- CPU clock frequency: `2.4 GHz` (varies by hardware)
- File descriptors: `fd=3` (ephemeral, session-local)
- Timestamps: `1735142477` (observer-dependent)
- Device paths: `/dev/sda1` (OS-specific)

**Representation**: `Observation` records
```json
{
  "key": "memory_address",
  "value": "0x7fff5fbff8a0",
  "source": "/proc/self/maps",
  "timestamp": "2025-12-25T18:30:00Z"
}
```

**Usage**:
- QoS decisions (schedule on fastest CPU)
- Debugging (show actual addresses)
- Tuning (measure actual performance)

**NOT used for**:
- Canonical identity
- Semantic truth
- Portable references

### Layer 2: Sphere (Semantics)

**Definition**: The mathematical, invariant, platform-independent truth

**Properties**:
- Canonical (stable across platforms)
- Platform-independent
- Relational (atoms + edges)
- Preserved through transformations

**Examples**:
- Atom identities: `accept`, `alphabet`, `left`
- Edge relations: `(start, CONNECTED_TO, state)`
- Graph topology: "DAG with 7 vertices, 12 edges"
- Fano closure: "{e₁, e₂, e₃} forms complete line 0"
- Genesis FORM: `AAAASSSAAAAASAAAASSSSSSSS`

**Representation**: `Graph` structure
```c
typedef struct {
  char* name;        // Atom name (canonical)
} Atom;

typedef struct {
  Atom src, dst;     // Relation (canonical)
  char* label;       // Edge type (canonical)
} Edge;

typedef struct {
  Atom* atoms;
  Edge* edges;
  size_t atom_count, edge_count;
} Graph;
```

**Usage**:
- Semantic validation (Fano-gate)
- Program equivalence checking
- Cross-platform execution
- Provenance verification

**Guarantees**:
- Same graph on Linux, Android, ESP32, Browser
- Same validation result everywhere
- Same projection output (modulo ball observations)

### Layer 3: Projection (Interface)

**Definition**: The derived, compatibility-preserving view of sphere onto ball

**Properties**:
- Idempotent: `proj(proj(x)) = proj(x)`
- Read-only (doesn't mutate graph)
- Lossy (discards information)
- Reversible (with limitations)

**Examples**:
- POSIX filesystem: Graph → directory tree
- SVG visualization: Graph → 2D coordinates
- GLB export: Graph → 3D mesh
- Hardware IDs: Capabilities → /dev nodes
- Genesis fingerprint: File → HW/V8/FORM

**Representation**: `Projection` function
```c
typedef void (*ProjectionFn)(Graph* graph, void* context);

// Example: SVG projection
void proj_svg_fano(Graph* g, FILE* out) {
  // Map graph atoms to 2D coordinates
  // Draw edges as lines
  // Output SVG XML
}
```

**Properties**:
```c
// Idempotence
SVG1 = proj_svg(graph);
SVG2 = proj_svg(SVG1);  // Invalid (type mismatch)
// But conceptually: SVG1 ≈ SVG2

// Read-only
Graph g1 = { ... };
proj_svg(&g1, stdout);
assert(graph_equals(&g1, &g1));  // g1 unchanged
```

**Usage**:
- Compatibility (run on POSIX systems)
- Visualization (show graph to humans)
- Interoperability (export to other tools)
- Debugging (observe graph state)

## Projection Examples

### Example 1: Hardware as Ball, Capabilities as Sphere

**Ball (Observable)**:
```json
{
  "cpu": {
    "model": "ARM Cortex-A53",
    "frequency": "1.4 GHz",
    "cores": 4,
    "address": "0x40000000"
  }
}
```

**Sphere (Canonical)**:
```
Atom: CPU_0
Edge: (CPU_0, HAS_CAPABILITY, cap:isa:armv8)
Edge: (CPU_0, HAS_CAPABILITY, cap:feature:neon)
Edge: (CPU_0, PART_OF, HOST)
```

**Projection (POSIX)**:
```
/sys/devices/system/cpu/cpu0/
/proc/cpuinfo → "processor: 0, model: ARM Cortex-A53"
```

**Key Insight**: The capabilities (`cap:isa:armv8`) are canonical. The device path (`/sys/...`) and frequency (`1.4 GHz`) are projections.

### Example 2: POSIX Filesystem as Projection

**Sphere (Canonical)**:
```
Atom: INODE_1234
Atom: DIR_ROOT
Atom: DIR_HOME
Atom: FILE_README

Edge: (DIR_ROOT, CONTAINS, DIR_HOME)
Edge: (DIR_HOME, CONTAINS, FILE_README)
Edge: (FILE_README, BACKED_BY, DATA_BLOB_5678)
```

**Ball (Observable)**:
```
Device: /dev/sda1
Inode: 1234
Physical block: 0x1a3f00
```

**Projection (POSIX)**:
```
/home/user/README.md → inode 1234
stat() → { dev: 2049, ino: 1234, size: 4096 }
```

**Operations**:
```c
// Canonical operation (sphere)
graph_add_edge(g, DIR_HOME, FILE_README, "CONTAINS");

// Projected operation (ball)
mkdir("/home/user");  // Creates atoms + edges
                      // Allocates inodes (observable)
```

**Idempotence**:
```bash
# Projection is idempotent
ls /home/user/  # Shows README.md
ls /home/user/  # Shows README.md (same result)
```

### Example 3: Genesis Fingerprinting

**Sphere (Canonical)**:
```
FORM skeleton: "AAAASSSAAAAASAAAASSSSSSSS_"
  (P₀/P₁ alternation pattern)
```

**Ball (Observable)**:
```json
{
  "hw": "0A:34:51:4B:4B:D1:0D:0D",  // Platform fingerprint
  "v": [207,89,90,13,20,178,28,12], // POSIX stat vector
  "stat": {
    "dev": 64820,      // Device ID (observable)
    "ino": 549130,     // Inode number (observable)
    "size": 6865,      // File size (observable)
    "mtime": 1766712077 // Modification time (observable)
  }
}
```

**Projection (Symbols)**:
```
ASCII:    AAAA SSS AAAAA S AAAA SSSSSSSS
Greek:    αααα ΩΩΩ ααααα Ω αααα ΩΩΩΩΩΩΩΩ
Octonion: e0e0e0e0 ΣΣΣ e0e0e0e0e0 Σ e0e0e0e0 ΣΣΣΣΣΣΣΣ
```

**Key Insight**:
- FORM is canonical (sphere)
- HW/V8 are observations (ball)
- Symbol choice is projection

**Comparison**:
```python
# Sphere comparison (canonical)
form1 = "AAAASSSAAAAASAAAASSSSSSSS_"
form2 = "AAAASSSAAAAASAAAASSSSSSSS_"
assert form1 == form2  # Structural equivalence

# Ball comparison (non-canonical)
hw1 = "0A:34:51:4B:4B:D1:0D:0D"  # Linux x86_64
hw2 = "1F:22:63:8A:9B:C3:1E:2F"  # Android ARM
assert hw1 != hw2  # Different hardware, same FORM
```

## Mathematical Formalization

### Projection as Function

```
P: Sphere → Ball

Where:
- Sphere = Set of canonical relations
- Ball = Set of observable facts
- P = Projection function
```

**Properties**:

1. **Idempotence**:
   ```
   P(P(x)) = P(x)
   ```

2. **Information Loss**:
   ```
   ∃ x,y ∈ Sphere: x ≠ y ∧ P(x) = P(y)
   ```

3. **Read-Only**:
   ```
   P(x) produces observations without mutating x
   ```

### Inverse Projection (Lifting)

```
P⁻¹: Ball → Sphere (partial function)

Where:
- Not all observations have canonical interpretation
- May be multiple valid lifts
```

**Example**:
```c
// Projection: Graph → POSIX
FILE* f = proj_posix_open(graph, INODE_1234);
int fd = fileno(f);  // Ball observation: fd=3

// Inverse (lifting): POSIX → Graph
// Given fd=3, which graph atom does it refer to?
// May be ambiguous without additional context
```

**Requirement**: Maintain **projection metadata** to enable lifting

```c
typedef struct {
  int fd;              // Ball observation
  Atom* canonical;     // Sphere reference
} ProjectionHandle;
```

## Design Patterns

### Pattern 1: Observations Never Canonical

**Problem**: Hardware IDs, addresses, timestamps are ball observations

**Solution**: Tag all observations with source

```c
typedef struct {
  Symbol key;          // What we're observing
  uint64_t value;      // Observed value
  Symbol source;       // Where we got it (/proc, sysfs, etc.)
  uint64_t timestamp;  // When we observed it
} Observation;

// Usage
Observation obs = {
  .key = "cpu_frequency",
  .value = 2400000000,  // 2.4 GHz
  .source = "/sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq",
  .timestamp = get_time()
};

// Never use obs.value as canonical identity!
```

### Pattern 2: Projection Context

**Problem**: Same graph can project differently based on context

**Solution**: Explicit projection context

```c
typedef struct {
  PlatformType platform;  // POSIX, ESP32, WASM
  OutputFormat format;    // SVG, GLB, JSON
  void* custom_params;    // Format-specific
} ProjectionContext;

void project(Graph* g, ProjectionContext* ctx) {
  switch (ctx->format) {
    case FORMAT_SVG:
      proj_svg_fano(g, ctx->custom_params);
      break;

    case FORMAT_GLB:
      proj_glb_merkaba(g, ctx->custom_params);
      break;

    case FORMAT_POSIX:
      proj_posix_filesystem(g, ctx->custom_params);
      break;
  }
}
```

### Pattern 3: Bidirectional Metadata

**Problem**: Need to go from projection back to canonical

**Solution**: Maintain projection → sphere mapping

```c
typedef struct {
  FILE* posix_handle;   // Ball projection
  Atom* atom;           // Sphere reference
  uint64_t inode;       // Ball observation (for debugging)
} FileHandle;

FileHandle* open_canonical(Graph* g, Atom* atom) {
  FileHandle* fh = malloc(sizeof(FileHandle));

  // Project atom to POSIX
  fh->posix_handle = proj_posix_open(g, atom);
  fh->atom = atom;  // Keep canonical reference!
  fh->inode = get_inode(fh->posix_handle);  // Observe

  return fh;
}

// Later: Retrieve canonical from projection
Atom* get_canonical(FileHandle* fh) {
  return fh->atom;  // Trivial with metadata
}
```

## Implementation Guidelines

### DO: Separate Sphere from Ball

**Good**:
```c
// Canonical graph operation
graph_add_edge(g, atom_start, atom_state, "TRANSITION");

// Observable projection
printf("Edge added at memory address %p\n", edge);
```

**Bad**:
```c
// Mixing ball and sphere
int edge_id = 42;  // Array index (ball)
edges[edge_id] = { start, state };  // Using ball as canonical
```

### DO: Tag Observations

**Good**:
```c
Observation freq = {
  .key = "cpu_frequency",
  .value = 2400000000,
  .source = "/sys/.../scaling_cur_freq",
  .timestamp = now()
};
```

**Bad**:
```c
uint64_t freq = 2400000000;  // No source, no timestamp
```

### DO: Explicit Projections

**Good**:
```c
SVG* svg = proj_svg_fano(graph);  // Explicit projection
GLB* glb = proj_glb_merkaba(graph);  // Different projection
```

**Bad**:
```c
void render(Graph* g) {
  // Implicit projection (which format?)
  output(g);
}
```

### DON'T: Project Ball to Ball

**Bad**:
```c
// Projecting an observation to another observation
char* addr_string = sprintf("0x%llx", address);  // Ball → Ball
```

**Good**:
```c
// Project sphere to ball
char* svg = proj_svg(graph);  // Sphere → Ball

// Or: Observe ball, tag as observation
Observation obs = {
  .key = "memory_address",
  .value = (uint64_t)address,
  .source = "program_counter",
  .timestamp = now()
};
```

### DON'T: Use Projections as Canonical

**Bad**:
```c
// Using projected value as canonical identity
int fd = open("/path/to/file", O_RDWR);
hash_table_insert(fd, value);  // fd is ball observation!
```

**Good**:
```c
// Use canonical atom as key
Atom* file_atom = graph_find_atom(g, "FILE_README");
hash_table_insert(file_atom, value);

// Track projection separately
projection_map[file_atom] = fd;  // Metadata
```

## Validation

### Sphere Invariants

**Must be preserved**:
- Graph connectivity (edges preserved)
- Fano closure (valid subset)
- Determinism (same input → same graph)
- Symbol agnosticism (shape preserved)

**Checking**:
```c
void validate_sphere(Graph* before, Graph* after) {
  assert(atom_count(before) == atom_count(after));
  assert(edge_count(before) == edge_count(after));
  assert(fano_closure(before) == fano_closure(after));
}
```

### Projection Invariants

**Must be preserved**:
- Idempotence: `P(P(x)) = P(x)`
- Read-only: Input graph unchanged
- Consistency: Same graph → same projection

**Checking**:
```c
void validate_projection(Graph* g) {
  Graph* g_copy = graph_clone(g);

  // Project
  SVG* svg = proj_svg_fano(g);

  // Check read-only
  assert(graph_equals(g, g_copy));

  // Check consistency
  SVG* svg2 = proj_svg_fano(g);
  assert(svg_equals(svg, svg2));
}
```

## Advanced Topics

### Multi-Level Projections

**Concept**: Project sphere → intermediate ball → final ball

```
Sphere (Graph)
    ↓ P1
Intermediate (CANB bytecode)
    ↓ P2
Ball (x86 machine code)
```

**Example**:
```c
Graph* g = parse_source("program.canasm");

// Level 1: Graph → CANB
CANB* bytecode = proj_canb(g);

// Level 2: CANB → Native
NativeCode* native = jit_compile(bytecode);

// Execute
native->execute();
```

**Property**: Composition preserves invariants
```
P2(P1(x)) maintains sphere invariants in final ball
```

### Projection Conflict Resolution

**Problem**: Multiple projections of same atom

```c
Atom* file = graph_find_atom(g, "FILE_README");

// Projection 1: POSIX
int fd1 = proj_posix_open(file);  // fd=3

// Projection 2: POSIX (again)
int fd2 = proj_posix_open(file);  // fd=4 (different!)
```

**Solution**: Track projection instances

```c
typedef struct {
  Atom* atom;
  int fd;
  int refcount;
} ProjectionInstance;

ProjectionInstance* get_or_create_projection(Atom* atom) {
  ProjectionInstance* pi = lookup(atom);
  if (pi) {
    pi->refcount++;
    return pi;
  }

  pi = malloc(sizeof(ProjectionInstance));
  pi->atom = atom;
  pi->fd = posix_open(atom);
  pi->refcount = 1;
  return pi;
}
```

### Projection Caching

**Optimization**: Cache projection results

```c
typedef struct {
  Graph* graph;
  uint64_t graph_hash;  // For invalidation
  SVG* cached_svg;
} ProjectionCache;

SVG* proj_svg_cached(Graph* g, ProjectionCache* cache) {
  uint64_t hash = graph_hash(g);

  if (cache->graph_hash == hash && cache->cached_svg) {
    return cache->cached_svg;  // Cache hit
  }

  // Cache miss: recompute
  SVG* svg = proj_svg_fano(g);
  cache->graph_hash = hash;
  cache->cached_svg = svg;
  return svg;
}
```

**Invalidation**: When graph changes, hash changes, cache invalidates

## Conclusion

The Ball/Sphere/Projection model provides:

1. **Separation of concerns**: Physics vs. semantics vs. interface
2. **Platform independence**: Sphere is same everywhere
3. **Compatibility**: Projections enable POSIX, hardware, etc.
4. **Clarity**: Explicit about what is canonical vs. observed

**Key Principle**:
> Numbers, addresses, timestamps, device IDs are **projections** (ball).
> Relations, atoms, graph structure are **canonical** (sphere).

This enables Tetragrammatron to run on any platform while preserving semantic correctness.

---

**See also**:
- [Architecture Overview](overview.md)
- [Hardware as Relations (RFC-0016)](../../tetragrammatron-os/docs/RFC-0016%20—%20Hardware-as-Relations%20Projection%20Layer%20(Normative).md)
- [POSIX as Projection (RFC-0017)](../../tetragrammatron-os/docs/RFC-0017%20—%20POSIX-as-Projection%20(Inode%20Relation%20Algebra)%20(Normative).md)
