# EVR Module Release Checklist

Use this checklist before cutting an EVR module release.

## Law completeness

- [x] EVR law inventory is complete and internally coherent.
- [x] Observational-only/non-authority boundary is explicit and testable.
- [x] Event family/kind inventory is explicit with implemented vs reserved separation.
- [x] No semantic TODO placeholders remain in normative EVR docs.

## Evidence-shape completeness

- [x] Every implemented `EventKind` has explicit evidence schema requirements.
- [x] Causal linkage requirements are explicit per outcome-bearing event kinds.
- [x] Source/layer legality map is explicit for implemented kinds.

## Fixture completeness

- [x] Golden fixture set complete for all implemented event families.
- [x] Negative fixture set complete for malformed/illegal event cases.
- [x] Determinism fixture set complete for shape/order/projection stability.
- [x] Reserved families are explicitly marked deferred, not silently omitted.

## Implementation/test completeness

- [x] Event emission tests pass for Core/Network/Hub touchpoints.
- [x] Event validation tests pass for malformed and illegal event cases.
- [x] Determinism tests pass for event shape and timeline projection stability.
- [x] Hub event inspection remains projection-only.
- [x] Event artifacts are canonically encodable and hash-stable.

## Release artifacts

- [x] `module-manifest.json` updated for EVR module release.
- [x] CHANGELOG contains EVR module section.
- [x] Release conformance statement includes EVR claims and deferred items.
- [x] Reference conformance checklist updated for EVR requirements.
