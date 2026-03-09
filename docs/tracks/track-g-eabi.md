# Track G Charter — EABI

## Objective

Define canonical execution/environment boundary framing over frozen ABI semantics.

## In

- Invocation/result/error envelopes
- Operation inventory and version table
- Context normalization
- Semantic vs execution failure separation
- Bundle and event framing

## Out

- Transport specifics (HTTP/SSE/WebSocket)
- Process/session/auth systems
- GUI/product behavior

## Acceptance gates

- ABI/EABI boundary is explicit and enforced
- Operation inventory complete and tested
- Envelope ordering deterministic
- Failure classes separated cleanly
- EABI demos/snapshots reproducible

## Milestones

- G1 laws/fixtures
- G2 operations inventory
- G3 pure eabi schemas/codecs
- G4 host mapping
- G5 events framing
- G6 bundle framing
- G7 fixtures/tests
- G8 implementation guide
- G9 demos/conformance
- G10 release artifacts
