# Changelog

## v1.0.0-semantic-baseline

### Track E (Revocation)
- Added pure revocation module (`src/revocation`) for canonical revocation records, deterministic validation, canonical encoding, and deterministic revocation-set verification.
- Integrated revocation semantics into capability verification so the same chain + revocation set + epoch yields the same typed outcome.
- Added Core revocation host path (`verifyRevocation`) and revocation-aware capability gating for resolve/adapter flows.
- Added EVR revocation event kinds:
  - `capability.revocation_recorded`
  - `capability.revocation_checked`
  - `capability.revocation_applied`
  - `capability.revocation_rejected`
- Added Hub revocation inspection pane (`revocation.verify`) as projection-only output over Core.
- Added revocation fixtures (golden/negative/determinism/integration), pure revocation tests, integration tests, and e2e tests.
- Added revocation demos and frozen snapshots:
  - valid chain then revoked
  - ancestor revoked in delegated chain
  - unauthorized revocation attempt rejected

### Baseline freeze
- Updated conformance snapshots, module manifest, and release conformance statement for semantic baseline freeze.
- Declared semantic baseline complete for deterministic meaning, identity, authority withdrawal, observability, and federation routing/convergence witness model.

## v0.4.0-federation

### Track C (Federation Breadth)
- Added pure federation module (`src/federation`) for descriptor/announcement validation, deterministic route-set derivation, deterministic arbitration, and convergence/divergence witnesses.
- Added explicit machine-level arbitration fields and tie-break order, plus canonical convergence/divergence witness shapes.
- Added federation fixtures (golden/negative/determinism/integration) and pure federation tests (`tests/federation/*`).
- Added network federation bridge (`src/network/federation.mjs`) and integrated HD-RPC announcement ingestion, route-set derivation, arbitration selection, and convergence checks.
- Added EVR federation event kinds:
  - `federation.announcement_received`
  - `federation.routeset_derived`
  - `federation.arbitration_selected`
  - `federation.convergence_witness`
  - `federation.divergence_witness`
- Added Hub federation inspection panes:
  - `federation.providers`
  - `federation.routeset`
  - `federation.arbitration`
  - `federation.convergence`
- Added federation demos and frozen snapshots:
  - multi-provider routing
  - deterministic arbitration
  - convergence witness
  - divergence witness
- Added federation integration tests for network/core/hub projection equivalence and capability-context passthrough.

## v0.3.0-evr

### Track B (EVR Doctrine)
- Added pure EVR module (`src/evr`) with event kind registry, envelope validator, canonical encoding, and deterministic projection view helpers.
- Defined implemented event families (`resolution`, `descriptor`, `capability`, `adapter`, `route`, `hub`) and reserved families (`federation`, `device`).
- Integrated Core event emission for resolve, descriptor lookup, capability verification, and adapter derivation outcomes.
- Integrated Network event emission for route lookup/call forwarding/failures.
- Integrated Hub event inspection surfaces (`events.timeline`, `events.detail`) as projection-only views.
- Added EVR fixtures (golden/negative/determinism/integration) and dedicated EVR tests.
- Added EVR end-to-end demos and frozen snapshots:
  - resolve trace
  - capability trace
  - route trace
- Added conformance coverage for EVR snapshots and deterministic timeline projection ordering.

## v0.2.0-capability

### Track A (Capability Model)
- Finalized capability law taxonomy and fixture matrix for golden/negative/determinism/integration classes.
- Implemented pure protocol capability verifier (`src/protocol/capability.mjs`) with deterministic typed outcomes and evidence.
- Integrated real capability verification in Core (`POST /verify-capability`) and removed placeholder non-semantic path.
- Added capability gating for `resolve` (`required_capability`) and a guarded adapter witness (`adapter:guarded-demo`).
- Hardened HD-RPC forwarding to carry capability context without semantic reinterpretation.
- Added Hub capability verification inspection pane (`capability.verify`) as projection-only output.
- Added full Track A test coverage across protocol/core/network/hub/integration/e2e.
- Added frozen capability demo snapshots:
  - valid delegation chain
  - expired chain denial
  - scope escalation denial

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
