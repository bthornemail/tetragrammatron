# Tetragrammatron Reference Stack

Minimal law-first reference implementation of the five-layer Tetragrammatron stack.

## Doctrine

- Identity is canonical.
- Authority is delegated.
- Credentials are adapters.
- Descriptors are discoverable.
- Presentation is projection.

## Layers

- `src/substrate` — NRR substrate (`put/get`, append-only log, replay, verify, bundles, checkpoints)
- `src/protocol` — DBC + DBC-IDL (stage calculus, canonical NormalForm, SID, descriptor)
- `src/core` — host + node transport mapping (faithful execution, canonical persistence)
- `src/network` — minimal HD-RPC (`call(SID, stage)`, route table, IPv6 canonical adapter)
- `src/hub` — read-mostly inspection shell

## Run

- Full tests: `npm test`
- Conformance snapshots: `npm run conformance`
- Hello demo: `npm run demo:hello`
- Node pipeline demo: `npm run demo:pipeline`
- Minimal federation proof: `npm run demo:federation`

## Frozen demo snapshots

Frozen snapshot fixtures live in `fixtures/demos/`:

- `hello.snapshot.json`
- `pipeline.snapshot.json`
- `federation.snapshot.json`

Snapshot conformance is enforced by `tests/conformance/demo-snapshots.test.mjs`.

## Release v0

- Tag: `v0.1.0-reference`
- Changelog: `CHANGELOG.md`
- Conformance statement: `RELEASE_CONFORMANCE.md`

## Implemented in this reference cut

- Deterministic canonical stack from substrate through hub
- Typed deterministic failure surfaces across protocol/core/network
- End-to-end runnable demos and reproducibility checks

## Deferred (intentional)

- Capability grant model completion
- EVR doctrine expansion
- Role-segmented hub
- Federation/discovery mesh breadth
- Embedded profile optimization work

## Conformance checklist

See `docs/reference-conformance-checklist.md`.

## Release policy

Post-baseline releases are module-gated (not feature-incremental):

- `docs/release-policy.md`
- `docs/modules/module-release-checklist.md`

## Independent tracks

Post-v0 work is intentionally split into independent tracks:

- Track A: capability / authority model (`docs/tracks/track-a-capability.md`)
- Track B: EVR doctrine (`docs/tracks/track-b-evr.md`)
- Track C: federation breadth (`docs/tracks/track-c-federation.md`)
- Track D: productization (`docs/tracks/track-d-productization.md`)

Cross-track dependency and contract rules:

- `docs/tracks/contract-map.md`
- `docs/tracks/workflow-rules.md`

## Roadmap diagram

- JSON Canvas: `docs/diagrams/protocol-roadmap.canvas`
- Static fallback: `docs/diagrams/protocol-roadmap.svg`

## Diagram profile

Tetragrammatron diagrams may use JSON Canvas as an optional visualization profile. Diagrams are projection-only and non-authoritative.

- `docs/diagrams/canvas-visualization-profile.md`
- `docs/diagrams/canvas-style-guide.md`
