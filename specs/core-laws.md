# Core Laws (Phase 3 Minimal Host)

- Core is a faithful execution host over Protocol + NRR.
- Core does not invent semantics beyond protocol outputs.
- Canonical artifacts are persisted as byte evidence in NRR.
- Descriptor lookup is discoverable from persisted canonical index artifacts.
- HTTP mapping is transport-only over host semantics.

## Error boundaries

- **Protocol reject**: lawful DBC/IDL stage failure (`Reject*`), surfaced as a typed non-success result.
- **Core boundary validation failure**: malformed host/API request (`invalid_request`, `invalid_stage`, `invalid_sid`), separate from protocol rejects.
- **Transport mapping failure**: HTTP adapter failure only (`internal_transport_mapping_error`), never used to represent protocol semantics.
