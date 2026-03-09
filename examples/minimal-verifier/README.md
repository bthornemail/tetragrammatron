# Minimal Independent Verifier

This example validates ABI fixture conformance using only the Conformance Kit artifacts.

## Scope

- Uses `conformance-kit.json` and ABI fixtures
- Performs lightweight structural checks and classification checks
- Does not import reference runtime internals (`src/core`, `src/protocol`, `src/network`, `src/hub`)

## Run

```bash
node examples/minimal-verifier/run-fixtures.mjs
```

## Non-goals

- Not a node
- Not a network participant
- Not a Hub
- Not a full runtime host
