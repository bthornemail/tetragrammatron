# Federation Module Release Checklist

Use this checklist before cutting a federation module release.

## Law completeness

- [x] Federation law inventory is complete and internally coherent.
- [x] Discovery/reachability/arbitration/convergence boundaries are explicit.
- [x] Non-authority boundary is explicit (no semantic invention).
- [x] Protocol/Capability/EVR/Hub interaction boundaries are explicit.
- [x] No semantic TODO placeholders remain in normative federation docs.

## Fixture completeness

- [x] Golden fixtures cover announce/discover/arbitrate/converge/call equivalence.
- [x] Negative fixtures cover conflict/unreachable/divergence/invalid descriptor/ambiguity/mismatch.
- [x] Determinism fixtures cover route set, arbitration, convergence, and divergence stability.
- [x] Integration fixtures cover end-to-end federation flow and Hub projection equivalence.
- [x] Implemented vs deferred deployment profile fixtures are explicit.

## Implementation/test completeness

- [x] Pure federation rule structures are implemented and tested.
- [x] Network discovery/announcement/arbitration tests are green.
- [x] Core-facing federation behavior tests are green where applicable.
- [x] Hub federation inspection tests are green and projection-only.
- [x] Convergence claims are backed by replay/canonical-artifact tests.
- [x] No placeholder discovery/arbitration semantics remain for implemented flows.

## Release artifacts

- [x] `module-manifest.json` updated for federation module release.
- [x] CHANGELOG contains federation module section.
- [x] Release conformance statement includes federation claims and deferred items.
- [x] Reference conformance checklist updated for federation requirements.
