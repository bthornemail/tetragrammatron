# Conformance Kit

The Conformance Kit is the public compatibility surface for independent implementations.

## Boundary

ABI defines the canonical semantic structures. EABI defines how those structures cross execution boundaries. EABI never redefines ABI.

## What it includes

- Machine-readable manifest: `conformance-kit.json`
- ABI fixtures: `fixtures/abi/{golden,negative,determinism}`
- EABI fixtures: `fixtures/eabi/{golden,negative,determinism}`
- Public transcripts: `fixtures/eabi/snapshots/*`
- Public verifier: `npm run conformance:kit`
- Export artifact: `releases/conformance-kit-v1.2.0.tar.gz`

## Version matrix

- Semantic baseline: `v1.0.0-semantic-baseline`
- ABI: `v1.1.0-abi`
- EABI: `v1.2.0-eabi`

## Minimum viable implementation levels

| Level | Must pass |
|---|---|
| Semantic reader | ABI schema + ABI golden fixtures |
| Semantic verifier | ABI schema + ABI golden/negative/determinism fixtures |
| Runtime host | EABI core operations + ABI conformance |
| Full node | EABI core + network + bundle + EVR fixtures |

## Pass criteria

- Fixture digests in `conformance-kit.json` match local fixture groups.
- Fixture classifications are explicit and valid (`success`, `semantic_failure`, `execution_failure`).
- Required operations are implemented for claimed host level.
- Transcript snapshots are present and hash-stable.

## Commands

- Verify public compatibility surface: `npm run conformance:kit`
- Export portable kit: `npm run conformance:kit:export`
