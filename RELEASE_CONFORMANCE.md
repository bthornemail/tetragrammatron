# Tetragrammatron Reference Stack v0 Conformance Statement

Release tag: `v0.1.0-reference`

This release is the foundational reference baseline for the minimal five-layer Tetragrammatron stack.

## Proven in this release

- Same meaning -> same identity -> same address.
- `resolve` is the only canonical write path.
- HD-RPC `call(SID, stage)` is semantic routing over Core, not semantic reinterpretation.
- Demo outputs are frozen and reproducibly checked via conformance tests.

## Implemented layer surface

- Substrate: NRR storage/replay/integrity/bundle/checkpoint.
- Protocol: DBC + DBC-IDL minimal deterministic calculus and identity projection.
- Core: DBC-NODE host contract and transport mapping.
- Network: minimal HD-RPC routing with canonical IPv6 adapter projection.
- Hub: read-mostly inspection shell.

## Intentionally deferred

- EVR doctrine completion.
- Broader federation/discovery mesh.
- Role-based hub segmentation.
- Embedded/runtime profile specialization.

## Release classification

This is a **reference-conformant baseline** (foundational completeness), not ecosystem-complete feature coverage.

---

# Capability Module Conformance Statement

Module version: `0.2.0-capability`

This module cut completes Track A capability semantics over the existing reference baseline.

## Proven in this module

- Delegated authority chains verify deterministically with typed outcomes and evidence.
- Capability verification does not alter SID, descriptor identity, or canonical protocol meaning.
- `POST /verify-capability` is runtime-complete (no placeholder `not_implemented` path).
- Capability-gated `resolve` and guarded adapter derivation fail/allow deterministically.
- HD-RPC forwards capability context as transport data; Network does not reinterpret capability semantics.
- Hub capability pane is projection-only and matches Core verifier output.

## Intentionally deferred after Track A

- Capability revocation semantics.
- Production PKI / hardware-backed cryptographic infrastructure.
- EVR doctrine completion.
- Federation discovery/arbitration breadth.
- Role-segmented hub workflows.
- Embedded/runtime profile specialization.

---

# EVR Module Conformance Statement

Module version: `0.3.0-evr`

This module cut completes Track B EVR doctrine over the existing reference baseline and capability module.

## Proven in this module

- Event families and kinds are explicit for implemented layers (`resolution`, `descriptor`, `capability`, `adapter`, `route`, `hub`).
- `EventKind` determines evidence shape via typed registry and validator.
- Event envelopes are canonically encodable and validation rejects are deterministic.
- Core and Network emit EVR events without altering protocol/capability semantics.
- Hub event timeline/detail panes are projection-only over emitted events.
- EVR demos are frozen and conformance-checked for deterministic trace shape.

## Intentionally deferred after Track B

- `federation.*` event family completion.
- `device.*` event family completion.
- Federation discovery/arbitration breadth.
- Role-segmented hub workflows.
- Embedded/runtime profile specialization.
