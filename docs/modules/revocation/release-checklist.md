# Revocation Module Release Checklist

Use this checklist before cutting a revocation module release.

## Law completeness

- [x] Revocation law inventory is complete and coherent.
- [x] Authority-withdrawal boundary is explicit and non-identity/non-meaning mutating.
- [x] Revocation authority and targeting rules are explicit.
- [x] Effective-epoch and propagation rules are explicit.
- [x] No semantic TODO placeholders remain in normative revocation docs.

## Fixture completeness

- [x] Golden fixtures cover direct and propagated revocation behavior.
- [x] Negative fixtures cover malformed/unauthorized/out-of-scope/target mismatch/conflict cases.
- [x] Determinism fixtures cover stable outputs and reject evidence.
- [x] Integration fixtures cover Core/Network/Hub revocation-aware behavior.

## Implementation/test completeness

- [x] Pure revocation structures/validation/verification tests are green.
- [x] Capability verification includes revocation in deterministic typed outcomes.
- [x] Core host/HTTP revocation paths are deterministic and equivalent.
- [x] EVR revocation events are emitted with stable evidence shapes.
- [x] Hub revocation panes are projection-only and match Core/EVR outputs.
- [x] No placeholder revocation behavior remains for implemented baseline model.

## Release artifacts

- [x] `module-manifest.json` updated for revocation module release.
- [x] CHANGELOG contains revocation module section.
- [x] Release conformance statement includes revocation claims and deferred items.
- [x] Reference conformance checklist updated for revocation requirements.
