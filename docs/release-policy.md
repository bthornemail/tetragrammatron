# Release Policy (Post `v1.0.0-semantic-baseline`)

## Baseline rule

`v1.0.0-semantic-baseline` is the frozen semantic baseline.

Future releases must not ship partial protocol surfaces. Releases are allowed only for completed, coherent modules.

## Release unit

A release unit is a **module**, not an incremental feature branch.

Examples of valid module release candidates:

- Capability model
- EVR doctrine
- Federation discovery/routing breadth
- Hub role segmentation

Examples of invalid release candidates:

- Partial capability verification
- Incomplete event family schema
- Early discovery experiments without convergence policy

## Required gates before release

A module release requires all of the following:

1. Law completeness
- Complete law inventory for the module.
- No semantic TODO placeholders.

2. Fixture completeness
- Golden fixtures.
- Negative fixtures.
- Determinism fixtures.

3. Test completeness
- Module tests green.
- Cross-layer integration tests green where module boundaries touch other layers.
- Conformance snapshots green.

4. Demo proof (when applicable)
- Module-level runnable demo proving the key claim.

5. Module manifest
- `module-manifest.json` present at repo root.
- Manifest JSON is valid and release-scoped fields are complete.

6. Patch-release spec stability
- Patch tags matching `vMAJOR.MINOR.PATCH` must not modify `specs/**`.
- Spec changes are allowed only on module releases with explicit scope/version intent.

## Track independence vs release discipline

Tracks may proceed independently:

- Track A — Capability
- Track B — EVR
- Track C — Federation
- Track D — Productization
- Track E — Revocation
- Track F — ABI
- Track H — Hub Roles

But releases happen only when one track yields a law-complete module.

## Experimental work

Experimental work must remain explicitly non-normative until promoted:

- under `docs/tracks/` planning artifacts, and/or
- under an `experimental` path if code prototypes are introduced.

Experimental artifacts must not redefine protocol semantics.

## Versioning guidance

Recommended progression after baseline:

- `v1.1.0-abi`
- `v1.2.0-eabi`
- subsequent module tags by scope (`capability`, `evr`, `federation`, `revocation`, `productization`)

Use module names in tag/release notes for clear scope boundaries.
