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

- Capability model completion.
- EVR doctrine completion.
- Broader federation/discovery mesh.
- Role-based hub segmentation.
- Embedded/runtime profile specialization.

## Release classification

This is a **reference-conformant baseline** (foundational completeness), not ecosystem-complete feature coverage.
