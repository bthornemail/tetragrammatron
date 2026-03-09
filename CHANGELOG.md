# Changelog

## v0.1.0-reference

### Substrate (NRR)
- Implemented content-addressed blob storage, append-only log, deterministic replay, integrity verification, bundle export/import, and checkpoint support.
- Added golden/negative/replay coverage for replay determinism, corruption handling, bundle integrity, and checkpoint replay equivalence.

### Protocol (DBC + DBC-IDL)
- Implemented stage-indexed resolution (`Realized`, `Closed`, `Normalized`, `Projected`) with deterministic typed rejects.
- Added canonical NormalForm handling, SID derivation from NormalForm, deterministic schema digest derivation, and descriptor projection/self-verification.

### Core (DBC-NODE host + transport mapping)
- Implemented faithful in-process host boundary over protocol + substrate.
- Added deterministic host validation taxonomy and explicit distinction between protocol reject, host validation failure, and transport mapping failure.
- Added HTTP transport mapping over host semantics for `/resolve`, `/sid/{digest}`, `/verify-capability`, and `/adapter/{label}/{sid}`.

### Network (HD-RPC minimal)
- Implemented minimal semantic routing `call(SID, stage)` over stable Core contract.
- Added deterministic route table, canonical `adapter:ipv6`, and compatibility-only `adapter:ipv4`.

### Hub (inspection shell)
- Implemented read-mostly inspection shell for resolve, descriptor, routing, store/replay, and local timeline events.
- Verified only resolve path mutates canonical state.

### Conformance and demos
- Added end-to-end demos: Hello Tetragrammatron, node pipeline, and minimal federation proof.
- Added frozen demo snapshots and conformance tests to detect snapshot drift.
- Added CI workflow covering tests and demo execution.
- Added reference conformance checklist documentation.
