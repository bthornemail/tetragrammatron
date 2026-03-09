# Track F Charter — ABI

## Objective

Define canonical semantic ABI structures/envelopes so independent implementations can reproduce baseline semantics without copying runtime internals.

## In

- Canonical semantic structures and envelopes
- Closed status taxonomies
- Deterministic encoding and ordering
- EVR evidence legality validation
- Structure and encoding conformance tests

## Out

- FFI/process/transport/socket/CLI/runtime mechanics (Track G)

## Acceptance gates

- ABI scope remains semantic-only
- Fixtures cover golden/negative/determinism for all ABI surfaces
- Cross-module ABI mapping tests pass
- ABI conformance snapshots are frozen

## Milestones

- F1 laws/fixtures
- F2 structures
- F3 pure abi module
- F4 cross-module mapping tests
- F5 snapshot conformance
- F6 implementation guide
- F7 ABI demos
- F8 release packaging
