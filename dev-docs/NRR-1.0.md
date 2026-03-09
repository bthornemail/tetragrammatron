# NRR-1.0 · Native Repository Runtime

**NRR-1.0 · Specification**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Draft Specification · Audience: runtime authors, embedded system implementers, node substrate authors  
Depends on: nothing  
Consumed by: DBC-1.2 · DBC-NODE-1.0 · HD-RPC-1.0

---

## Abstract

The Native Repository Runtime (NRR) defines the minimal canonical substrate for deterministic semantic infrastructure. It specifies content-addressed storage, an append-only log, deterministic replay, integrity verification, and export/import bundles — nothing more.

NRR does not define schema law, semantic normalization, identity derivation, capability verification, routing, or UI behavior. Those are concerns of higher layers. NRR defines only the substrate on which those layers may stand.

> **A hash-indexed append-only log with deterministic replay.**

NRR is intentionally small enough to run on bare metal. It targets ESP32, embedded Linux, WASM, and desktop. It is Git-optional by construction: Git may serve as an adapter for transport and replication, but it is not a dependency and not required for correctness.

The system does not assume networks, clocks, consensus, or cryptography beyond hash identity. It still provides replayability, verification, composability, and auditability.

---

## 0. Canonical Vocabulary

| Term | Meaning |
|---|---|
| blob | an immutable byte string stored under its content-addressed ref |
| ref | the hash of a blob's bytes — the canonical address of a blob |
| entry | a fixed-size log record: phase, type, and ref |
| log | the ordered, append-only sequence of entries |
| boundary | a set of refs that define the active semantic scope for replay |
| replay | the deterministic reconstruction of state from an ordered log and blob set |
| bundle | a portable, self-contained export of blobs and a log segment |
| checkpoint | a log position from which replay may resume, producing the same final state as full replay |
| `HashRef` | a hash-prefixed content reference, e.g. `sha256:<hex>` |
| NRR runtime | a conforming implementation of this specification |

---

## Document Structure

| Section | Role |
|---|---|
| §1 — Scope | What NRR defines and what it explicitly does not define |
| §2 — Design Goals | The properties a conforming NRR runtime must satisfy |
| §3 — Core Laws | The six invariants of the substrate |
| §4 — Object Model | Blob, Entry, Log, BoundarySet, Bundle |
| §5 — Required API | The four required operations and optional extensions |
| §6 — Replay Semantics | The normative replay algorithm |
| §7 — File Format | Binary log, objects store, boundary file, manifest |
| §8 — Integrity Rules | Blob, log, boundary, and bundle verification |
| §9 — Hash Algorithm | Default, alternatives, and pluggability |
| §10 — Git Compatibility | Git as an optional adapter |
| §11 — MCU / Embedded Profile | Implementation notes for constrained devices |
| §12 — Relation to the Stack | How NRR sits below DBC, IDL, NODE, and HD-RPC |
| §13 — Conformance | Test classes and the minimal initial test set |
| §14 — Security | Threat model and mitigations at the substrate level |

---

## 1. Scope

### 1.1 What NRR Defines

- content-addressed blob storage (§4.1, §5)
- append-only log with fixed-size binary entries (§4.2, §4.3, §7.3)
- deterministic replay from log and blob set (§6)
- integrity verification for blobs, logs, boundaries, and bundles (§8)
- export/import bundles for offline replication (§4.5, §5.2)
- a pluggable hash algorithm with SHA-256 as default (§9)
- an optional Git compatibility adapter (§10)
- an embedded/MCU implementation profile (§11)

### 1.2 What NRR Explicitly Does Not Define

| Out of scope | Defined in |
|---|---|
| Schema law and symbolic admission | DBC-1.2 |
| Stage-indexed resolution | DBC-1.2 |
| Semantic normalization | DBC-1.2 |
| Identity derivation (SID) | DBC-IDL-1.2 |
| Capability grants and authority | DBC-IDL-1.2 |
| Adapter credential derivation | DBC-IDL-1.2 |
| Identity descriptors | DBC-IDL-1.2 |
| Node HTTP endpoints | DBC-NODE-1.0 |
| Semantic routing (IPv6 adapter) | HD-RPC-1.0 |
| Federation protocol | HD-RPC-1.0, DBC-NODE-1.0 |
| GUI and event registry | HD-RPC-GUI-1.0, HD-RPC-EVR-1.0 |

NRR is the substrate. It does not know what the blobs mean. Higher layers assign meaning; NRR ensures the blobs are retrievable, the log is ordered, and replay is deterministic.

---

## 2. Design Goals

A conforming NRR runtime must satisfy all of the following goals.

| Goal | Statement |
|---|---|
| Minimality | The required interface fits on bare metal. No heap, no OS, no network, no clock required for the core operations. |
| Portability | The same file format and API runs on ESP32, embedded Linux, WASM, and desktop without modification to the substrate. |
| Git-optional | Git may be used as a transport and replication adapter but is not a dependency. Correctness does not require Git. |
| Binary safety | All log records and blob storage are binary-safe. Text encoding is not assumed. |
| Replayability | State is reconstructible from the append-only log and blob set alone. No external state is required. |
| Determinism | The same log and blob set always produce the same replay result, regardless of runtime, language, or architecture. |
| Integrity-first | Every get, every replay step, and every bundle import verifies hash correctness. Unverified blobs are not served. |
| No assumed infrastructure | The runtime does not assume networks, clocks, consensus, or cryptographic authority beyond hash identity. |

---

## 3. Core Laws

### 3.1 Content Address Law

```
ref(bytes) = hash(bytes)
```

Every blob is addressed by the hash of its bytes. No two distinct byte strings may share a ref if the hash function is collision-resistant. A ref is the canonical address of a blob.

### 3.2 Append-Only Law

```
log entries are never modified or deleted
```

The log is immutable except by appending new entries at the tail. No in-place mutation of prior entries is permitted. No entry may be removed. A conforming runtime that receives a write request to modify a prior entry must reject it.

### 3.3 Replay Law

```
same(log, blobs) → same(state)
```

Given the same ordered log and the same blob set, replay produces the same final state, on any conforming runtime, in any language, on any architecture. Replay is the correctness oracle. If two runtimes diverge on the same inputs, at least one is non-conforming.

### 3.4 Artifact Immutability Law

```
once stored under ref R, content(R) never changes
```

A blob stored under a given ref is immutable. A conforming runtime that receives a request to overwrite an existing ref with different bytes must reject it. If the bytes are identical, the operation is idempotent and may succeed silently.

### 3.5 Transport Neutrality Law

```
NRR correctness does not depend on any transport
```

The substrate defines no network protocol. Git, HTTP, UART, MQTT, file copy, or bundle exchange may move artifacts between runtimes. None of these are required. Correctness is defined only by the Content Address Law, the Append-Only Law, and the Replay Law.

### 3.6 Authority Neutrality Law

```
NRR does not define identity or authorization
```

The substrate stores canonical artifacts. It has no concept of who wrote a blob, whether an actor is authorized to append an entry, or what the semantic meaning of any blob is. Authority and identity are higher-layer concerns.

---

## 4. Object Model

### 4.1 Blob

A blob is an immutable byte string stored under its content-addressed ref.

```
Blob :=
  ref:        HashRef           // hash(bytes), canonical address
  bytes:      ByteString        // the stored content
  media_type: MediaType         // optional; may be null
```

The `media_type` is informational. It does not affect addressing. Two blobs with identical bytes but different declared media types are the same blob under the same ref.

### 4.2 EntryType

```
EntryType :=
  boundary  // sets or replaces the active boundary scope
  interior  // an artifact to be applied to state within the current boundary
  guarantee // an assertion about state at this log position
```

Higher layers may define additional entry types. A conforming NRR runtime must store and replay entries with unknown types without modification, treating them as opaque records.

### 4.3 Entry

An entry is a fixed-size, binary-safe log record.

```
Entry :=
  phase:  uint32     // monotonic phase counter
  type:   EntryType  // boundary | interior | guarantee | <extension>
  ref:    HashRef    // content-addressed blob reference
```

On flash or disk, the wire encoding is:

```
[4 bytes: phase, big-endian uint32]
[1 byte:  type]
[32 bytes: raw SHA-256 hash bytes]
= 37 bytes per entry (fixed size with SHA-256)
```

Fixed-size records allow O(1) seek by entry index. This property must be preserved if a runtime uses a different hash algorithm — the record size must remain fixed for the chosen algorithm.

### 4.4 Log

The log is the ordered, append-only sequence of all entries.

```
Log := [Entry_0, Entry_1, ..., Entry_N]
```

The log is the source of truth for replay. No other data structure — including the boundary file or any checkpoint — is authoritative over the log. All other structures are derived from or consistent with the log.

### 4.5 BoundarySet

The boundary set is the current set of active boundary refs.

```
BoundarySet := [HashRef_0, HashRef_1, ..., HashRef_K]
```

The boundary set is a replay optimization: it records the state of active boundaries at a given log position so that replay may resume from a checkpoint without scanning from entry 0. The boundary set is derived from the log. If the log and boundary set disagree, the log is correct.

### 4.6 Bundle

A bundle is a portable, self-contained export of blobs and a log segment.

```
Bundle :=
  manifest:     BundleManifest
  blobs:        [Blob]
  log_segment:  [Entry]
```

```
BundleManifest :=
  nrr_version:    "1.0"
  hash_algorithm: "sha256" | "blake3"
  log_format:     "nrr-log-bin-v1"
  entry_count:    uint32
  blob_count:     uint32
  segment_start:  uint32   // first entry index in this segment
  segment_end:    uint32   // last entry index in this segment (inclusive)
  manifest_hash:  HashRef  // sha256 of manifest JSON with manifest_hash = null
```

Bundles are the primary mechanism for offline replication, migration, and cross-runtime transfer.

---

## 5. Required API

### 5.1 Required Operations

Every conforming NRR runtime must implement these four operations.

```
put(bytes) -> HashRef
```
Store bytes. Compute ref = hash(bytes). Store blob under ref. Return ref. Idempotent: if ref already exists with identical bytes, return ref without error.

```
get(ref) -> bytes
```
Retrieve bytes stored under ref. Verify hash(bytes) = ref before returning. If ref is not found, return a not-found error. If hash verification fails, return an integrity error — never return unverified bytes.

```
append(entry) -> entry_index
```
Append entry to the log tail. Return the zero-based index of the appended entry. Reject if type is invalid. Reject if ref does not exist in the blob store (to prevent dangling refs in the log).

```
log() -> [Entry]
```
Return the ordered log from entry 0 to the current tail. The returned sequence must be identical to what was appended, in append order, with no gaps.

### 5.2 Optional Extended Operations

These operations are not required for conformance but are recommended for practical implementations.

```
has(ref) -> bool
```
Returns true if the blob exists and hash verification would pass. Does not load the full blob if it can be avoided.

```
entries(from_index, limit) -> [Entry]
```
Returns a slice of the log starting at `from_index`, up to `limit` entries. Used for efficient streaming replay and incremental sync.

```
replay(from_checkpoint?) -> state
```
Runs the normative replay algorithm (§6) from the given checkpoint, or from entry 0 if no checkpoint is provided. Returns the final state.

```
export(refs...) -> Bundle
```
Creates a bundle containing the specified blobs and the log segment that references them. If no refs are specified, exports the entire repository.

```
import(bundle) -> [HashRef]
```
Validates and imports a bundle. Verifies the manifest, all blob hashes, and log segment consistency. Returns the list of refs successfully imported.

```
compact(policy) -> CompactionReport
```
Applies a compaction policy to the blob store. The log is never compacted. Only unreferenced blobs (blobs not referenced by any log entry) may be removed, subject to the provided policy.

```
verify() -> IntegrityReport
```
Runs a full integrity check: verifies all blob hashes, verifies log record framing, verifies all log refs exist in the blob store, verifies boundary consistency.

---

## 6. Replay Semantics

### 6.1 Normative Replay Algorithm

The following is the normative replay procedure. A conforming runtime must produce the same output as this algorithm for the same inputs.

```
procedure replay(log, blob_store, from_checkpoint = null):

  if from_checkpoint is not null:
    state    = from_checkpoint.state
    boundary = from_checkpoint.boundary
    start    = from_checkpoint.entry_index + 1
  else:
    state    = empty
    boundary = empty
    start    = 0

  for i from start to len(log) - 1:
    entry = log[i]

    if entry.type == boundary:
      blob = blob_store.get(entry.ref)   // must not fail
      boundary = load_boundary(blob)

    else if entry.type == interior:
      blob = blob_store.get(entry.ref)   // must not fail
      state = apply(state, blob)
      assert valid(state, boundary)      // must not fail

    else if entry.type == guarantee:
      blob = blob_store.get(entry.ref)
      assert verify_guarantee(state, blob)  // must not fail

    // unknown entry types: load and store, do not apply to state

  return state
```

### 6.2 Replay Requirements

A conforming implementation must:

- process entries in strict log order, from `start` to the log tail
- load every referenced blob via `get(ref)`, which verifies the hash before returning
- reject replay if any `get(ref)` call fails (missing blob or hash mismatch)
- reject replay if any `assert valid(state, boundary)` fails
- reject replay if any `assert verify_guarantee(state, blob)` fails
- produce identical output from identical inputs on every run

### 6.3 Checkpoint Validity

A checkpoint is valid only if it was produced by a prior replay from entry 0 (or from another valid checkpoint) and the log has not been corrupted since the checkpoint was taken. A runtime may use a checkpoint to skip already-replayed entries, but must be able to fall back to full replay from entry 0 if the checkpoint is unavailable.

**Checkpoint law:** `replay(from_checkpoint) = replay(from_entry_0)` for any valid checkpoint. This must be verified before a checkpoint is trusted.

### 6.4 Idempotence

Replay is idempotent. Running replay twice from the same starting state on the same log produces the same result. This follows from the Replay Law (§3.3) and the Artifact Immutability Law (§3.4).

---

## 7. File Format

### 7.1 Repository Layout

```
/repo/
  objects/
    ab/
      cd/
        abcd1234...   (blob content)
  log.bin             (binary append-only log)
  boundary.bin        (current boundary refs, replay optimization)
  manifest.json       (repository metadata)
```

On embedded systems:
- `objects/` may map to flash sectors addressed by hash prefix
- `log.bin` may be ring-buffered in a circular flash region
- blobs may be optional if content is streamed and not stored
- `boundary.bin` may be omitted if memory allows full replay from scratch

### 7.2 `objects/` — Content Store

Blobs are stored in a two-level directory structure derived from the ref:

```
objects/<hex[0:2]>/<hex[2:4]>/<full_hex>
```

The file content is the raw blob bytes. The file name is the ref. No metadata is stored in the file itself.

On systems without a filesystem, blobs may be stored in a flat hash table, a B-tree index over flash, or any structure that supports O(1) or O(log n) lookup by ref.

### 7.3 `log.bin` — Binary Append-Only Log

The log is a binary file of fixed-size records.

**Record format (SHA-256, 37 bytes per record):**

```
Offset  Length  Field
0       4       phase (big-endian uint32)
4       1       type (0x01=boundary, 0x02=interior, 0x03=guarantee)
5       32      ref (raw 32-byte SHA-256 hash)
= 37 bytes total
```

**For BLAKE3 (32-byte hash, same wire size):** identical layout. The record size is the same because BLAKE3 also produces 32-byte digests at the default output length.

Fixed-size records allow:
- O(1) seek to entry N: `file_offset = N * 37`
- no framing overhead
- direct mmap on supported systems
- ring-buffer operation on MCUs

The file is opened in append-only mode. Writes are sequential. No random writes to existing records.

### 7.4 `boundary.bin` — Active Boundary State

The boundary file stores the current set of active boundary refs as a sequence of raw hash bytes.

```
[32 bytes: ref_0][32 bytes: ref_1]...[32 bytes: ref_K]
```

This file is a replay optimization only. It must never be treated as authoritative over the log. A runtime that loses or corrupts `boundary.bin` must be able to reconstruct the boundary state by replaying `log.bin` from entry 0.

### 7.5 `manifest.json` — Repository Metadata

```json
{
  "created_by":        "<implementation_id>",
  "features":          ["bundle", "checkpoint", "compact", "verify"],
  "hash_algorithm":    "sha256",
  "log_format":        "nrr-log-bin-v1",
  "manifest_version":  "NRR-1.0",
  "boundary_format":   "nrr-boundary-bin-v1",
  "nrr_version":       "1.0"
}
```

Keys are in canonical lexicographic order. The manifest is informational metadata. A runtime must not refuse to open a repository whose manifest differs in non-structural ways (e.g., different `created_by`), but must reject a repository whose `nrr_version` or `log_format` is not understood.

---

## 8. Integrity Rules

### 8.1 Blob Integrity

Every `get(ref)` must verify `hash(bytes) == ref` before returning. A runtime must never return unverified bytes. A hash mismatch is an integrity error that must be surfaced to the caller; it must not be silently swallowed.

### 8.2 Log Integrity

A log verifier must confirm:

- every record is exactly the expected fixed size (no partial records at tail)
- `type` byte is a known value (0x01, 0x02, 0x03, or a declared extension)
- every `ref` in every entry resolves to a blob via `get(ref)` without error
- `phase` values are monotonically non-decreasing

### 8.3 Boundary Integrity

Every ref in the current `BoundarySet` must resolve to a valid blob via `get(ref)`. A boundary ref that resolves to a missing or corrupted blob is an integrity violation.

### 8.4 Bundle Integrity

A bundle import must:

1. verify `manifest_hash` matches the SHA-256 of the manifest JSON with `manifest_hash = null`
2. verify `nrr_version` and `log_format` are supported
3. verify every blob's hash matches its ref
4. verify the log segment record framing is valid
5. verify all refs in the log segment resolve to blobs included in the bundle

A bundle that fails any of these checks must be rejected entirely. Partial imports are not permitted.

---

## 9. Hash Algorithm

### 9.1 Default

**SHA-256** is the default and mandatory baseline. Every conforming NRR runtime must support SHA-256.

### 9.2 Optional Alternatives

| Algorithm | Use case | Notes |
|---|---|---|
| BLAKE3 | performance-sensitive runtimes | same 32-byte output length as SHA-256; identical wire format |
| SHA-3-256 | post-quantum preference | 32-byte output; same wire format |
| CRC32 | ultra-low resource testing only | NOT collision-resistant; must not be used in production |

### 9.3 Algorithm Declaration

The hash algorithm in use must be declared in `manifest.json`. A runtime must not silently change the hash algorithm for an existing repository. Changing the algorithm requires creating a new repository and migrating.

### 9.4 Pluggability

A runtime may expose a pluggable hash interface. The plug point is:

```
hash_function: ByteString -> [u8; 32]
```

The output must always be exactly 32 bytes, so that the fixed-size log record format (§7.3) is preserved regardless of which algorithm is selected.

---

## 10. Git Compatibility

Git is an optional adapter. NRR does not require Git. A runtime that never touches Git is fully conforming.

When Git is used, the mapping is:

```
NRR.put(bytes)    →  git hash-object -w <bytes>
NRR.get(ref)      →  git cat-file blob <ref>
NRR.log()         →  git log (commit history)
NRR.append(entry) →  git commit (with entry blob as tree content)
```

Git serves as:

- a **transport** — `git push`/`git pull` replicate NRR state between runtimes
- a **replication layer** — existing Git hosting (GitHub, Gitea, Forgejo) stores NRR objects
- a **human UI** — `git log`, `git show`, `git diff` provide operator visibility into NRR history

Git is **not**:

- required for correctness
- the source of truth for replay
- required for identity, authority, or semantic meaning

A runtime that uses Git as its object store must not allow Git operations that violate the NRR Append-Only Law (e.g., `git rebase`, `git reset --hard`, force-push to prior commits).

---

## 11. MCU / Embedded Profile

### 11.1 Minimum Requirements

A device running the NRR embedded profile must be able to:

1. compute SHA-256 (hardware-accelerated or software)
2. write to sequential flash storage (NOR or NAND)
3. read from sequential flash storage
4. seek by byte offset (for O(1) log entry lookup)

No heap allocation, no OS, no network, no real-time clock is required for the core operations.

### 11.2 Recommended Stack for ESP32

| Layer | Choice |
|---|---|
| Language | C / Rust / Zig |
| Hash | BLAKE3 or mbedTLS SHA-256 |
| Log storage | NVS partition or custom append-log on flash |
| Object store | SPIFFS or LittleFS with hash-prefix path |
| Transport | optional UART / WiFi / BLE |
| Validation | PCG integrity check or Fano table |

### 11.3 Ring-Buffer Log Operation

On MCUs with limited flash endurance, the log may be ring-buffered:

- the log wraps when the physical storage is full
- the oldest entries are overwritten
- a checkpoint must be taken before any entry is overwritten

**Ring-buffer law:** before overwriting entry N, the runtime must have taken a valid checkpoint at a position ≥ N. A runtime that overwrites entries without a checkpoint violates the Replay Law.

### 11.4 Optional Blob Storage

On severely constrained devices, blobs may not be stored locally. The log records their refs. Blobs are retrieved on demand from a peer node or bundle. In this configuration:

- `put(bytes)` computes and returns the ref but does not store locally
- `get(ref)` fetches from a peer
- replay requires network access

This is conforming under the Transport Neutrality Law, provided the runtime clearly documents that it is operating in streaming mode.

### 11.5 WASM Profile

WASM runtimes may operate the full NRR interface using:

- `crypto.subtle.digest` for SHA-256
- `IndexedDB` or `OPFS` for blob storage and log
- `Uint8Array` for binary log records

The same file format and API apply. WASM runtimes must not use text-only log encodings.

---

## 12. Relation to the Stack

NRR is the substrate. The rest of the DBC / HD-RPC stack builds above it.

```
HD-RPC-GUI-1.0 · HD-RPC-EVR-1.0   ← projection shell · event registry
HD-RPC-1.0                         ← umbrella architecture
DBC-NODE-1.0                        ← node protocol (uses NRR for artifact + descriptor store)
DBC-IDL-1.2                         ← identity layer (SID derived from NRR NormalForms)
DBC-1.2                             ← calculus (normalizes documents; results stored in NRR)
NRR-1.0                             ← substrate (blobs · log · replay)
```

### 12.1 NRR and DBC-1.2

DBC-1.2 normalizes semantic documents. The resulting `NormalForm` artifacts are stored as blobs in NRR. The SRCall and SRResult envelopes are also stored as blobs, keyed by their content-addressed refs. NRR knows nothing about what these blobs mean. DBC knows nothing about how they are stored.

### 12.2 NRR and DBC-NODE-1.0

DBC-NODE-1.0 defines the artifact store and descriptor store that a node maintains. Both stores are NRR repositories. The node-layer law **Artifact Immutability** is NRR's Artifact Immutability Law (§3.4) applied at the node boundary. The node-layer law **Descriptor Non-Authority** is enforced by the higher layer; NRR does not know that a blob is a descriptor.

### 12.3 NRR and DBC-IDL-1.2

SIDs are derived from NormalForms. NormalForms are stored in NRR. NRR stores the blobs from which SIDs are derived, but NRR does not compute SIDs and has no concept of identity.

### 12.4 NRR and HD-RPC-1.0

HD-RPC requires deterministic replay and transport neutrality. Both are provided by NRR. The HD-RPC federation convergence property depends on the Replay Law: the same calculus inputs always produce the same NormalForms, which are stored in NRR and retrieved without alteration.

---

## 13. Conformance

### 13.1 Test Classes

| Class | What it proves |
|---|---|
| Golden | required operations produce correct, deterministic results |
| Negative | integrity violations are rejected with correct errors |
| Portability | same inputs produce same outputs on two independent runtimes |
| Adapter | Git adapter round-trip preserves correctness |
| Replay | replay from checkpoint equals replay from entry 0 |

### 13.2 Test Definitions

#### Golden Tests

| ID | Test | Input | Expected |
|---|---|---|---|
| NRR-G-01 | blob store/retrieve round-trip | `put(bytes)` → `get(ref)` | returned bytes == original bytes |
| NRR-G-02 | append/log round-trip | append N entries → `log()` | same N entries in same order |
| NRR-G-03 | replay from empty state | log with boundary + interior entries | expected final state |
| NRR-G-04 | bundle export/import | export → import on fresh runtime | same blobs and replay result |
| NRR-G-05 | idempotent put | `put(bytes)` twice | same ref, no error |

#### Negative Tests

| ID | Reject case | Expected error |
|---|---|---|
| NRR-N-01 | `get` on missing ref | not-found error |
| NRR-N-02 | blob hash mismatch | integrity error, bytes not returned |
| NRR-N-03 | malformed log record (wrong size) | integrity error |
| NRR-N-04 | log record with unknown type byte (no extension) | store without error; skip in replay |
| NRR-N-05 | boundary ref resolves to missing blob | integrity error during replay |
| NRR-N-06 | append entry with non-existent ref | reject with dangling-ref error |
| NRR-N-07 | bundle with mismatched manifest hash | reject entire bundle |
| NRR-N-08 | overwrite existing ref with different bytes | reject with immutability error |

#### Portability Tests

| ID | Scenario |
|---|---|
| NRR-P-01 | same bundle replays identically on two runtimes (same language, different machine) |
| NRR-P-02 | same bundle replays identically on two runtimes (different language implementations) |
| NRR-P-03 | SHA-256 test vectors: known input produces expected ref |

#### Adapter Tests

| ID | Scenario |
|---|---|
| NRR-A-01 | Git adapter round-trip: `put` via NRR, retrieve via `git cat-file` |
| NRR-A-02 | file bundle export/import round-trip: same replay result |

#### Replay Tests

| ID | Scenario |
|---|---|
| NRR-R-01 | replay from checkpoint produces same state as replay from entry 0 |
| NRR-R-02 | repeated replay (no new entries) is idempotent |
| NRR-R-03 | replay on MCU ring-buffer log with valid checkpoint |

### 13.3 Minimal Initial Profile

The smallest first deployable conformance set:

```
NRR-G-01, NRR-G-02, NRR-G-03
NRR-N-01, NRR-N-02, NRR-N-03, NRR-N-06
NRR-P-03
NRR-R-01
```

This covers core blob correctness, log append/read, basic replay, primary integrity violations, the SHA-256 vector test, and checkpoint validity.

---

## 14. Security

### 14.1 Threat Model

NRR operates at the substrate level. Its security assumptions are minimal by design:

- **hash collision resistance** — the system is secure against blob substitution if the hash function is collision-resistant
- **no cryptographic authentication** — NRR does not verify who wrote a blob or appended an entry; that is a higher-layer concern
- **no access control** — NRR does not enforce who may call `put`, `get`, or `append`; access control is a higher-layer or system-layer concern

### 14.2 Hash Integrity

The only security property NRR guarantees is content integrity: `get(ref)` returns the exact bytes that were stored under `ref`, or it fails. This guarantee is as strong as the collision resistance of the chosen hash function.

A runtime that serves unverified bytes violates the Content Address Law and provides no integrity guarantee.

### 14.3 Append-Only as Audit Trail

The Append-Only Law (§3.2) provides an audit trail by construction. History cannot be rewritten without detectable log corruption. Higher layers (DBC-NODE-1.0 Artifact Immutability, DBC-EVR-1.0 Event Immutability) depend on this property.

### 14.4 Algorithm Agility Risk

Changing the hash algorithm invalidates all existing refs. NRR does not define a migration protocol for algorithm changes. The manifest declares the algorithm; implementations must not silently accept blobs hashed under a different algorithm than the one declared.

CRC32 must not be used in any deployment where integrity against adversarial tampering is required.

### 14.5 Bundle Import Validation

Bundles imported from untrusted sources must pass all integrity checks defined in §8.4 before any blob is written to the local store. A partial import that writes some blobs and then fails is a conformance violation; imports are atomic.

---

## Summary

### The Laws

| Law | Statement |
|---|---|
| Content Address Law | `ref(bytes) = hash(bytes)` |
| Append-Only Law | log entries are never modified or deleted |
| Replay Law | same log and blob set → same state, on any conforming runtime |
| Artifact Immutability Law | once stored under ref R, content never changes |
| Transport Neutrality Law | correctness does not depend on any transport |
| Authority Neutrality Law | NRR does not define identity or authorization |

### The Stack Position

```
DBC-1.2 + DBC-IDL-1.2 + DBC-NODE-1.0 + HD-RPC-1.0
                 ↓
            NRR-1.0
     (blobs · log · replay · integrity · bundles)
```

NRR is the floor. Everything above it can build on a substrate that guarantees content integrity, append-only history, and deterministic replay — without assuming networks, clocks, consensus, or cryptographic authority.

---

## Changelog

| Version | Change |
|---|---|
| NRR-1.0 | Initial specification. Six core laws, five-type object model, four required API operations, normative replay algorithm, binary log format, integrity rules, SHA-256 default with pluggable hash, optional Git adapter, ESP32/WASM embedded profile, 18-test conformance surface, security section. |

---

*NRR-1.0 · Native Repository Runtime · Brian Thorne · bthornemail*
