# Cross-Track Contract Map

Baseline floor: `v1.0.0-semantic-baseline`.

## Dependency rules

- Track A (Capability) depends on Protocol/Core contracts only.
- Track B (EVR) depends on Core/Network observable outputs only.
- Track C (Federation) depends on Core/Network contracts, and may consume Track A outputs only after Track A contract freeze.
- Track D (Productization) depends on stable APIs/contracts from A/B/C and must not define semantics.
- Track E (Revocation) depends on Track A capability semantics and must not alter identity/protocol meaning.
- Track H (Hub Roles) depends on Core/Network/EVR contracts and must not define semantic authority.

## Frozen contracts

- Core request/result envelopes and deterministic error taxonomy.
- Network `call(SID, stage)` semantics and adapter non-authority rules.
- Capability/revocation authority semantics and typed reject taxonomy.
- Conformance snapshots and demo acceptance claims.

## Change policy

- Any frozen-contract change requires a versioned RFC and migration note.
- Cross-track changes must include explicit contract-impact section in PR description.
- No track may silently reinterpret another track's output semantics.
