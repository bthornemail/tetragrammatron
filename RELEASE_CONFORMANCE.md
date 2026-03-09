# Tetragrammatron Semantic Baseline Conformance Statement

Release tag: `v1.0.0-semantic-baseline`

This release freezes the foundational semantic baseline for Tetragrammatron.

## Proven in this release

- Same meaning -> same identity -> same address.
- `resolve` is the only canonical write path.
- Capability verification is deterministic and typed.
- Revocation deterministically withdraws delegated authority without mutating SID or canonical meaning.
- HD-RPC `call(SID, stage)` is semantic routing over Core, not semantic reinterpretation.
- EVR event kinds are deterministic observational evidence; EventKind determines evidence shape.
- Federation route derivation/arbitration and convergence/divergence witnesses are deterministic for equivalent inputs.
- Demo outputs are frozen and reproducibly checked via conformance tests.

## Implemented baseline surface

- Substrate: NRR storage/replay/integrity/bundle/checkpoint.
- Protocol: DBC + DBC-IDL deterministic calculus and identity projection.
- Core: DBC-NODE host contract + HTTP mapping.
- Network: HD-RPC routing + federation breadth for local deterministic witness profile.
- Hub: read-mostly inspection shell.
- Capability: delegated authority verification.
- Revocation: deterministic authority withdrawal.
- EVR: deterministic event taxonomy + evidence-shape validation.

## Intentionally deferred after semantic baseline

- Production PKI / hardware-backed cryptographic infrastructure.
- Revocation distribution policies beyond deterministic record evaluation.
- EVR `device.*` family completion.
- Wide-area federation mesh optimization policies.
- Embedded/mobile constrained profiles.
- Role-segmented hub workflows.

## Release classification

This release is a **semantic baseline freeze**: foundationally stable semantics with deferred ecosystem/product breadth.

---

# Reference Stack v0 Conformance Statement

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
