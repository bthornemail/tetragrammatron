# Track E Charter — Revocation

## Objective

Complete deterministic authority withdrawal semantics as baseline-completion work.

## In

- Canonical revocation record structure and deterministic encoding
- Deterministic revocation verification outcomes and typed evidence
- Capability-chain interaction and ancestor propagation behavior
- Core/Network/Hub integration as projection over canonical verification

## Out

- PKI ecosystem design
- HSM/vendor integration
- Online mutable revocation services
- Policy engine breadth beyond deterministic baseline semantics

## Acceptance gates

- Revocation does not mutate SID or canonical meaning
- Same chain + same revocation set + same epoch => same outcome
- Revocation-aware capability checks are deterministic across Core/Network/Hub
- Revocation demos and snapshots are frozen and conformance-checked

## Milestones

- E1: laws + fixtures + checklist
- E2: pure revocation structures/validation/verification
- E3: protocol/core/network/hub integration
- E4: demos/conformance/release artifacts
