# Capability Module Release Checklist

Use this checklist before cutting a capability module release.

## Law completeness

- [ ] Capability law inventory is complete and internally coherent.
- [ ] Boundary doctrine is explicit: capability does not define identity or descriptor semantics.
- [ ] No semantic TODO placeholders remain in normative capability docs.

## Fixture completeness

- [ ] Golden fixture set complete.
- [ ] Negative fixture set complete.
- [ ] Determinism fixture set complete.
- [ ] Fixture evidence shapes are explicit and stable.

## Implementation/test completeness

- [ ] Golden/negative/determinism fixtures are implemented as automated tests.
- [ ] Core verifier surface returns typed deterministic outcomes.
- [ ] Cross-layer tests (Core/Network/Hub touchpoints) are added where capability checks are consumed.
- [ ] No placeholder verification states remain (except explicitly documented deferred states during pre-release development).

## Release artifacts

- [ ] `module-manifest.json` updated for capability module release.
- [ ] CHANGELOG contains capability module section.
- [ ] Release conformance statement includes capability claims and deferred items.
- [ ] Conformance checklist updated with capability module requirements.
