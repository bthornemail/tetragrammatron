# Tetragrammatron Semantic Baseline

Deterministic law-first reference implementation of the Tetragrammatron semantic baseline.

## Doctrine

- Identity is canonical.
- Authority is delegated.
- Credentials are adapters.
- Descriptors are discoverable.
- Presentation is projection.

## Layers and modules

- `src/substrate` — NRR substrate (`put/get`, append-only log, replay, verify, bundles, checkpoints)
- `src/protocol` — DBC + DBC-IDL + capability verification
- `src/revocation` — canonical revocation records and deterministic authority-withdrawal verification
- `src/core` — host + node transport mapping (faithful execution, canonical persistence)
- `src/network` — HD-RPC routing + federation bridge
- `src/federation` — pure federation rules (descriptors, announcements, route sets, arbitration, convergence)
- `src/evr` — event doctrine module (kind registry, envelope validation, canonical encoding)
- `src/hub` — read-mostly inspection shell

## Run

- Full tests: `npm test`
- Conformance snapshots: `npm run conformance`
- All demos: `npm run demo:all`

### Individual demos

- `npm run demo:hello`
- `npm run demo:pipeline`
- `npm run demo:federation`
- `npm run demo:capability:valid`
- `npm run demo:capability:expired`
- `npm run demo:capability:scope`
- `npm run demo:evr:resolve`
- `npm run demo:evr:capability`
- `npm run demo:evr:route`
- `npm run demo:federation:multi`
- `npm run demo:federation:arbitration`
- `npm run demo:federation:convergence`
- `npm run demo:federation:divergence`
- `npm run demo:revocation:revoked`
- `npm run demo:revocation:ancestor`
- `npm run demo:revocation:unauthorized`

## Frozen demo snapshots

Frozen snapshot fixtures live in `fixtures/demos/`:

- `hello.snapshot.json`
- `pipeline.snapshot.json`
- `federation.snapshot.json`
- `capability-valid.snapshot.json`
- `capability-expired.snapshot.json`
- `capability-scope.snapshot.json`
- `evr-resolve-trace.snapshot.json`
- `evr-capability-trace.snapshot.json`
- `evr-route-trace.snapshot.json`
- `federation-multi-provider.snapshot.json`
- `federation-deterministic-arbitration.snapshot.json`
- `federation-convergence-witness.snapshot.json`
- `federation-divergence-witness.snapshot.json`
- `revocation-valid-then-revoked.snapshot.json`
- `revocation-ancestor-revoked.snapshot.json`
- `revocation-unauthorized-attempt.snapshot.json`

Snapshot conformance is enforced by `tests/conformance/demo-snapshots.test.mjs`.

## Release status

- Baseline tag target: `v1.0.0-semantic-baseline`
- Previous reference tag: `v0.1.0-reference`
- Changelog: `CHANGELOG.md`
- Conformance statements: `RELEASE_CONFORMANCE.md`

## Implemented in semantic baseline

- Deterministic canonical stack from substrate through hub
- Typed deterministic failure surfaces across protocol/core/network
- Capability delegation verification with deterministic reject taxonomy
- Revocation-aware authority withdrawal with deterministic typed outcomes
- EVR event taxonomy/validation with deterministic timeline projections
- Federation discovery/arbitration/convergence witnesses for local deterministic profile
- End-to-end runnable demos and reproducibility checks

## Deferred (intentional)

- Production PKI / hardware-backed cryptographic infrastructure
- Revocation distribution policies beyond deterministic record evaluation
- EVR `device.*` family completion
- Role-segmented hub
- Wide-area federation mesh optimization
- Embedded profile optimization work

## Conformance checklist

See `docs/reference-conformance-checklist.md`.

## Release policy

Post-baseline releases are module-gated (not feature-incremental):

- `docs/release-policy.md`
- `docs/modules/module-release-checklist.md`

## Independent tracks

Post-baseline work is intentionally split into independent tracks:

- Track A: capability / authority model (`docs/tracks/track-a-capability.md`)
- Track B: EVR doctrine (`docs/tracks/track-b-evr.md`)
- Track C: federation breadth (`docs/tracks/track-c-federation.md`)
- Track D: productization (`docs/tracks/track-d-productization.md`)
- Track E: revocation (`docs/tracks/track-e-revocation.md`)

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
