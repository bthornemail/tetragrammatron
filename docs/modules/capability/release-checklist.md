# Capability Module Release Checklist

Use this checklist before cutting a capability module release.

## Law completeness

- [x] Capability law inventory is complete and internally coherent.
- [x] Boundary doctrine is explicit: capability does not define identity or descriptor semantics.
- [x] No semantic TODO placeholders remain in normative capability docs.

## Fixture completeness

- [x] Golden fixture set complete.
- [x] Negative fixture set complete.
- [x] Determinism fixture set complete.
- [x] Fixture evidence shapes are explicit and stable.

## Implementation/test completeness

- [x] Golden/negative/determinism fixtures are implemented as automated tests.
- [x] Core verifier surface returns typed deterministic outcomes.
- [x] Cross-layer tests (Core/Network/Hub touchpoints) are added where capability checks are consumed.
- [x] No placeholder verification states remain (except explicitly documented deferred states during pre-release development).

## Release artifacts

- [x] `module-manifest.json` updated for capability module release.
- [x] CHANGELOG contains capability module section.
- [x] Release conformance statement includes capability claims and deferred items.
- [x] Conformance checklist updated with capability module requirements.
