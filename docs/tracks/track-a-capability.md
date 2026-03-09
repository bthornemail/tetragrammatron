# Track A Charter — Capability / Authority

## Objective

Implement deterministic capability grants, delegation chains, and verification semantics without changing identity law.

## In

- Canonical `CapabilityGrant` schema
- Delegation chain verification boundary
- Deterministic verification result taxonomy
- Fixture matrix for valid/expired/revoked/broken/malformed grants

## Out

- EVR doctrine expansion
- Federation peer discovery mesh
- UI role segmentation

## Acceptance gates

- `POST /verify-capability` transitions from `not_implemented` to typed deterministic verifier
- Negative fixtures prove chain break handling and signature failures
- Conformance docs updated with capability checks

## Milestones

- A1: grant schema + canonical encoding + fixtures
- A2: verifier + endpoint contract + integration tests
- A3: release note and contract freeze for capability API
