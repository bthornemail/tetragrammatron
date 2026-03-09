# Hub Roles Fixture Matrix (Track H)

## Golden

- H-G01 Provider opens A/B/E and resolves via `/resolve`.
- H-G02 Broker verifies capability and forwards call with verbatim-forward witness.
- H-G03 Consumer resolves and inspects deterministic stage/result envelope.
- H-G04 User opens F only and receives read-only projection surfaces.
- H-G05 Agent can inspect all workspaces and event-family feeds.

## Negative

- H-N01 Role leakage: forbidden workspace is rejected deterministically.
- H-N02 Endpoint mismatch: panel endpoint metadata diverges from law map.
- H-N03 User mutation attempt (`resolve`) rejected as `read_only_projection`.
- H-N04 Broker rewrite attempt produces failed verbatim witness.
- H-N05 Malformed EVR event is not accepted as valid display evidence.

## Determinism

- H-D01 Same role + same inputs => same visible workspace payload.
- H-D02 Same route forward request => same broker witness digest pair.
- H-D03 Same event stream inputs => same family-filtered workspace feed.

## Integration

- H-I01 Role/workspace map is enforced by shell command authorization.
- H-I02 Workspace panels map to existing endpoint/EVR surfaces only.
- H-I03 Projection portal remains mutation-free.
- H-I04 Broker forwarding over HD-RPC remains semantically equivalent to direct call.
