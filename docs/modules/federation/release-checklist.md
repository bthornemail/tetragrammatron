# Federation Module Release Checklist

Use this checklist before cutting a federation module release.

## Law completeness

- [ ] Federation law inventory is complete and internally coherent.
- [ ] Discovery/reachability/arbitration/convergence boundaries are explicit.
- [ ] Non-authority boundary is explicit (no semantic invention).
- [ ] Protocol/Capability/EVR/Hub interaction boundaries are explicit.
- [ ] No semantic TODO placeholders remain in normative federation docs.

## Fixture completeness

- [ ] Golden fixtures cover announce/discover/arbitrate/converge/call equivalence.
- [ ] Negative fixtures cover conflict/unreachable/divergence/invalid descriptor/ambiguity/mismatch.
- [ ] Determinism fixtures cover route set, arbitration, convergence, and divergence stability.
- [ ] Integration fixtures cover end-to-end federation flow and Hub projection equivalence.
- [ ] Implemented vs deferred deployment profile fixtures are explicit.

## Implementation/test completeness

- [ ] Pure federation rule structures are implemented and tested.
- [ ] Network discovery/announcement/arbitration tests are green.
- [ ] Core-facing federation behavior tests are green where applicable.
- [ ] Hub federation inspection tests are green and projection-only.
- [ ] Convergence claims are backed by replay/canonical-artifact tests.
- [ ] No placeholder discovery/arbitration semantics remain for implemented flows.

## Release artifacts

- [ ] `module-manifest.json` updated for federation module release.
- [ ] CHANGELOG contains federation module section.
- [ ] Release conformance statement includes federation claims and deferred items.
- [ ] Reference conformance checklist updated for federation requirements.
