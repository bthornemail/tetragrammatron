# NRR Laws (Phase 1 Substrate)

## Enforced laws

1. **Content Address Law**
   - `ref(bytes) = sha256(bytes)`
   - Stored as `sha256:<hex>`.

2. **Append-Only Law**
   - `log.bin` is only written by tail append of fixed-size records.
   - Existing records are never rewritten by runtime APIs.

3. **Replay Law**
   - Same ordered log + same blobs + same reducer => same rebuilt state.

4. **Artifact Immutability Law**
   - A blob address is immutable.
   - Re-put of identical bytes is idempotent.

5. **Authority Neutrality Law**
   - Substrate stores bytes and ordered refs only.
   - No semantic or identity authority exists in substrate code.

## Boundaries for this slice

Out of scope in Phase 1 bounded task:
- DBC normalization and semantics
- SID derivation and descriptors
- Capability verification
- Network routing and adapters
- GUI/EVR projection concerns
- Checkpoint persistence

## Integrity split (Phase 1b)

- **Store integrity**
  - Object bytes verify against content hash.
  - Log framing and references are valid against local objects.
- **Bundle integrity**
  - Manifest hash, log framing, and bundled blobs all verify before import.
  - Import rejects missing refs or hash mismatches without rewriting refs.
- **Replay determinism**
  - Determinism is defined by `same log + same blobs + same reducer -> same rebuilt state`.

## Checkpoint vocabulary (Phase 1c)

- **full replay**: replay from log entry `0`.
- **checkpoint replay**: replay from a checkpoint artifact plus log suffix.
- **replay equivalence**: full replay and checkpoint replay produce identical final state.
- **checkpoint artifact**: content-addressed metadata artifact containing checkpoint boundary and snapshot ref.
- **checkpoint boundary**: log entry index captured by checkpoint artifact.
