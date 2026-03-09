# Release Policy (Post `v0.1.0-reference`)

## Baseline rule

`v0.1.0-reference` is the frozen architectural baseline.

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

## Track independence vs release discipline

Tracks may proceed independently:

- Track A — Capability
- Track B — EVR
- Track C — Federation
- Track D — Productization

But releases happen only when one track yields a law-complete module.

## Experimental work

Experimental work must remain explicitly non-normative until promoted:

- under `docs/tracks/` planning artifacts, and/or
- under an `experimental` path if code prototypes are introduced.

Experimental artifacts must not redefine protocol semantics.

## Versioning guidance

Recommended progression after baseline:

- `v0.2.0-capability`
- `v0.3.0-evr`
- `v0.4.0-federation`

Use module names in tag/release notes for clear scope boundaries.
