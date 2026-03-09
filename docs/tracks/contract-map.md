# Cross-Track Contract Map

Baseline floor: `v0.1.0-reference`.

## Dependency rules

- Track A (Capability) depends on Protocol/Core contracts only.
- Track B (EVR) depends on Core/Network observable outputs only.
- Track C (Federation) depends on Core/Network contracts, and may consume Track A outputs only after Track A contract freeze.
- Track D (Productization) depends on stable APIs/contracts from A/B/C and must not define semantics.

## Frozen contracts

- Core request/result envelopes and deterministic error taxonomy.
- Network `call(SID, stage)` semantics and adapter non-authority rules.
- Conformance snapshots and demo acceptance claims.

## Change policy

- Any frozen-contract change requires a versioned RFC and migration note.
- Cross-track changes must include explicit contract-impact section in PR description.
- No track may silently reinterpret another track's output semantics.
