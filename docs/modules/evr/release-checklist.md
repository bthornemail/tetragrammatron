# EVR Module Release Checklist

Use this checklist before cutting an EVR module release.

## Law completeness

- [ ] EVR law inventory is complete and internally coherent.
- [ ] Observational-only/non-authority boundary is explicit and testable.
- [ ] Event family/kind inventory is explicit with implemented vs reserved separation.
- [ ] No semantic TODO placeholders remain in normative EVR docs.

## Evidence-shape completeness

- [ ] Every implemented `EventKind` has explicit evidence schema requirements.
- [ ] Causal linkage requirements are explicit per outcome-bearing event kinds.
- [ ] Source/layer legality map is explicit for implemented kinds.

## Fixture completeness

- [ ] Golden fixture set complete for all implemented event families.
- [ ] Negative fixture set complete for malformed/illegal event cases.
- [ ] Determinism fixture set complete for shape/order/projection stability.
- [ ] Reserved families are explicitly marked deferred, not silently omitted.

## Implementation/test completeness

- [ ] Event emission tests pass for Core/Network/Hub touchpoints.
- [ ] Event validation tests pass for malformed and illegal event cases.
- [ ] Determinism tests pass for event shape and timeline projection stability.
- [ ] Hub event inspection remains projection-only.
- [ ] Event artifacts are canonically encodable and hash-stable.

## Release artifacts

- [ ] `module-manifest.json` updated for EVR module release.
- [ ] CHANGELOG contains EVR module section.
- [ ] Release conformance statement includes EVR claims and deferred items.
- [ ] Reference conformance checklist updated for EVR requirements.
