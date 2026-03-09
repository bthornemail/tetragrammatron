# Network Laws (Phase 4 Minimal HD-RPC)

- Semantic Addressing: network calls are addressed by SID and stage.
- Deterministic Route Derivation: route resolution is deterministic for a fixed route table.
- Adapter Non-Authority: adapter outputs project connectivity only and never redefine SID/descriptor semantics.
- Transport Neutral Routing: HD-RPC forwards to Core host without reinterpretation.
- IPv6 Canonicality: `adapter:ipv6` is canonical network projection.
- IPv4 Compatibility: `adapter:ipv4` is compatibility-only and non-authoritative.
