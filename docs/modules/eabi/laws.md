# EABI Laws (Track G)

ABI defines the canonical semantic structures. EABI defines how those structures cross execution boundaries. EABI never redefines ABI.

## Scope boundary

EABI is execution-boundary only. It frames canonical ABI structures for invocation and exchange.

## Laws

1. Execution Boundary
- EABI carries invocation/result framing only.

2. ABI Transparency
- ABI structures are carried verbatim; EABI does not summarize or reinterpret semantic payloads.

3. Explicit Versioning
- Every envelope carries `eabi_version`; every operation is versioned.

4. Canonical Envelope Ordering
- Invocation/result/error/stream envelopes use fixed canonical field order.

5. Semantic/Execution Failure Separation
- Semantic failures stay inside ABI result payloads in successful EABI envelopes.
- Execution failures use EABI error envelopes.
- An EABI host must never translate an ABI semantic failure into an EABI execution error.

6. Context Normalization
- Unknown context keys are ignored unless required by operation contract.

7. Operation Version Stability
- Operation meaning does not change within same major EABI version.

8. Transport Neutrality
- EABI framing is transport-neutral and transport-agnostic.

9. Deterministic Framing
- Same invocation -> same envelope shape and content.

10. Import/Export Replayability
- Bundle framing preserves NRR logical replayability and ref integrity.

11. Event Framing Neutrality
- Event framing metadata never mutates embedded EVR event payloads.
