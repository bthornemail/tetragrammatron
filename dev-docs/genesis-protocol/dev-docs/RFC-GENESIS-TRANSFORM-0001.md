RFC-GENESIS-TRANSFORM-0001
GENESIS Canonical Transform Specification

**Status:** Standards Track (Draft)
**Layer:** 3+ (Visualization / Projection)
**Updates:** None
**Obsoletes:** None
**Intended Audience:** Visualization engines, 3D renderers, simulation systems

---

## 1. Abstract

This document defines a canonical deterministic transform from GENESIS atoms (file metadata + provenance) to spatial coordinates (x, y, z), scale, and rotation.

The transform:
- Is **deterministic**: same input atom always produces same output
- **Factors through projection**: depends only on canonical atom fields
- Is **portable**: reference implementations in C, JavaScript, Python, Guile Scheme
- Is **formally verified**: Lean proof that execution factors through CanonAtom projection

This specification operates at Layer 3 or above, building on Layer 0 (existence), Layer 1 (selection), and Layer 2 (identity).

---

## 2. Conformance Language

The key words MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT, MAY, and OPTIONAL in this document are to be interpreted as described in RFC 2119.

---

## 3. Canonical Atom Input (Normative)

An implementation MUST accept atoms with the following canonical fields:

```
atom = {
  path:  String,           // file path
  realm: String,           // top-level folder or realm tag
  depth: Nat,              // directory depth
  role:  String,           // "kernel"|"source"|"org"|"asset"|...
  stat:  {                 // POSIX stat fields
    size:  Nat,            // file size in bytes
    mtime: Nat,            // modification time (epoch seconds)
    inode: Nat             // inode number
  },
  schema: {                // layer classification
    class: String,         // "private"|"protected"|"public"
    epoch: Nat             // version/epoch
  }
}
```

An implementation MAY accept additional fields but MUST NOT use them in the transform computation.

---

## 4. Canonical Transform Output (Normative)

An implementation MUST produce a transform with the following canonical fields:

```
Transform = {
  position: (x, y, z),     // 3D spatial coordinates
  scale:    s,             // uniform scale factor
  rotation: rotY           // Y-axis rotation in radians
}
```

### 4.1 Packed Buffer Representation

For GPU/WASM/FFI interop, implementations SHOULD support a packed `Float32` representation:

```
[x, y, z, s, rotY]
```

5 floats, 20 bytes total.

---

## 5. Transform Algorithm (Normative)

### 5.1 Realm Index Derivation

The realm index MUST be derived deterministically from the `realm` string:

```
realmIndex = hash(realm) mod realmCount
```

Where:
- `hash()` SHOULD be FNV-1a 32-bit for portability
- `realmCount` is a parameter (default: 8)

### 5.2 Cylindrical Position (X, Z)

The X and Z coordinates MUST be computed via cylindrical projection:

```
angle = 2π * (realmIndex / realmCount)
x = cos(angle) * radius
z = sin(angle) * radius
```

Where `radius` is a parameter (default: 8.0).

### 5.3 Vertical Position (Y)

The Y coordinate MUST combine depth, time, and schema class:

```
y = depth * depthStep + mtime * timeScale + classOffset
```

Where:
- `depthStep` is a parameter (default: 2.0)
- `timeScale` is a parameter (default: 0.0000005)
- `classOffset` is derived from `schema.class`:
  - `"private"`: 0.0
  - `"protected"`: 0.25
  - `"public"`: 0.5

### 5.4 Scale

The scale MUST be logarithmic with respect to file size:

```
s = max(minScale, log(size + 1) * sizeScale)
```

Where:
- `minScale` is a parameter (default: 0.3)
- `sizeScale` is a parameter (default: 0.001)

### 5.5 Rotation

The Y-axis rotation MUST be derived from inode:

```
rotY = 2π * ((inode mod rotMod) / rotMod)
```

Where `rotMod` is a parameter (default: 360).

---

## 6. Default Parameters

Implementations MUST support the following default parameter set:

```
{
  realmCount: 8,
  radius:     8.0,
  depthStep:  2.0,
  timeScale:  0.0000005,
  sizeScale:  0.001,
  minScale:   0.3,
  rotMod:     360
}
```

Implementations MAY allow parameter overrides.

---

## 7. Factorization Through Projection (Normative)

An implementation MUST guarantee that the transform depends ONLY on the canonical atom fields listed in Section 3.

**Formal statement:**

If two atoms `a` and `b` have identical canonical projections:
```
π(a) = π(b)
```

Then they MUST produce identical transforms:
```
transform(a) = transform(b)
```

Where `π` projects to:
```
CanonAtom = {
  realmIndex: Nat,
  realmCount: Nat,
  depth:      Nat,
  mtime:      Nat,
  size:       Nat,
  inode:      Nat,
  classTag:   Nat    // 0=private, 1=protected, 2=public
}
```

A formal Lean proof of this property is provided in Appendix A.

---

## 8. Role to Geometry (Informative)

While the transform computes position/scale/rotation, the choice of 3D geometry is orthogonal and MAY be determined by the `role` field:

```
role → geometry (suggested mapping):
  "kernel"  → octahedron
  "org"     → icosahedron
  "source"  → box
  "asset"   → sphere
  default   → box
```

This mapping is NOT part of the canonical transform and implementations MAY use different schemes.

---

## 9. Reference Implementations

### 9.1 JavaScript

Reference implementation: `genesis-transform.js`

Suitable for: Three.js, WebGPU, browser-based visualization

**Key functions:**
- `genesisTransform(atom, opts)` → `{position, scale, rotation, realmIndex}`
- `toBuffer(transform)` → `Float32Array[5]`
- `geometryForRole(role)` → geometry hint

### 9.2 C

Reference implementation: `genesis_transform.h`

Suitable for: POSIX systems, native performance, GPU upload

**Key functions:**
- `genesis_transform(atom*, opts...)` → `GenesisTransform`
- `genesis_to_buffer(transform*, out5[])` → packed floats

### 9.3 Python

Reference implementation: `genesis_transform.py`

Suitable for: batch processing, data pipeline, tooling

**Key functions:**
- `genesis_transform(atom, opts)` → `{pos, scale, rotY, realmIndex}`
- `to_buffer(transform)` → `[x,y,z,s,rotY]`

### 9.4 Guile Scheme

Reference implementation: `genesis-transform.scm`

Suitable for: integration with `genesis-protocol/genesis.org` literate source

**Key functions:**
- `(genesis-transform atom . opts)` → association list
- `(to-buffer transform)` → list of floats

---

## 10. Security Considerations

The transform is deterministic and stateless. It does NOT:
- Execute code from input files
- Perform file I/O beyond reading metadata
- Depend on network state
- Modify the filesystem

Implementations SHOULD validate that `realmCount` and `rotMod` are non-zero to avoid division by zero.

---

## 11. IANA Considerations

This document has no IANA actions.

---

## 12. References

### 12.1 Normative References

- RFC-0001-TPPM: Two-Primitive Projection Model
- GENESIS.org: Canonical system declaration

### 12.2 Informative References

- docs/AGENTS.md: Consensus simulation theory
- RFC 2119: Key words for RFCs

---

## Appendix A: Lean Formalization (Informative)

A complete formal proof in Lean 4 that the transform factors through projection is provided in `formal/GenesisTransform.lean`.

**Key theorems:**

1. **Factorization theorem:**
   ```lean
   theorem transform_factors (p : Params) :
     transform p = (fun a => T p (π a)) := rfl
   ```

2. **Projection respect:**
   ```lean
   theorem transform_respects_projection (p : Params) (a b : Atom)
     (h : π a = π b) : transform p a = transform p b
   ```

3. **Uniqueness through projection:**
   ```lean
   theorem unique_factor_through_projection
     (p : Params) (f : Atom → Transform)
     (hf : ∀ a, f a = T p (π a)) :
     ∀ a b, π a = π b → f a = f b
   ```

This proves that the transform is **uniquely determined** by the projection `π`, making it impossible for implementation details or extra fields to influence the output.

---

## Appendix B: FNV-1a Hash (Informative)

The FNV-1a 32-bit hash is used for realm index derivation:

```
FNV_offset_basis = 0x811c9dc5
FNV_prime        = 0x01000193

hash = FNV_offset_basis
for each byte b in input:
  hash = hash XOR b
  hash = hash * FNV_prime
return hash AND 0xFFFFFFFF
```

This hash was chosen for:
- Simplicity (no dependencies)
- Portability (pure arithmetic)
- Good distribution for short strings

---

## Appendix C: Integration with genesis.sh (Informative)

Future versions of `genesis.sh` MAY provide:

```bash
# Emit JSONL with transform fields added
./genesis.sh emit-transform-jsonl atoms.jsonl > transformed.jsonl

# Emit binary buffer for GPU upload
./genesis.sh emit-buffer-bin atoms.jsonl > scene.bin

# Emit CSV for data analysis
./genesis.sh emit-xyz.csv atoms.jsonl > positions.csv
```

These commands are NOT part of this specification but demonstrate integration paths.

---

**End of RFC-GENESIS-TRANSFORM-0001**
