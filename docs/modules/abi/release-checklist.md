# ABI Release Checklist

## F1-F2 docs

- [x] ABI laws complete.
- [x] ABI fixture matrix complete.
- [x] ABI structure inventory complete.
- [x] ABI scope excludes EABI/runtime concerns.

## F3 pure ABI layer

- [x] `src/abi/schema.mjs` complete.
- [x] `src/abi/validate.mjs` complete.
- [x] `src/abi/encode.mjs` complete.
- [x] `src/abi/errors.mjs` complete.
- [x] `src/abi/kinds.mjs` complete.

## F4-F5 conformance

- [x] ABI fixtures (golden/negative/determinism) frozen.
- [x] ABI tests pass across protocol/identity/capability/revocation/evr/federation.
- [x] ABI conformance snapshot test passes.
- [x] Structure and encoding conformance both proven.

## F6-F8 release

- [x] ABI implementation guide complete.
- [x] ABI demo scripts frozen and reproducible.
- [x] `module-manifest.json` updated for ABI.
- [x] `CHANGELOG.md` includes Track F.
- [x] `RELEASE_CONFORMANCE.md` includes ABI conformance section.
