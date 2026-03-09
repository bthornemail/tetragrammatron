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
