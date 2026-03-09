# ABI Laws (Track F)

## Scope boundary

ABI describes canonical semantic structures and outcomes. ABI does not include execution/runtime/process/transport concerns.

## Laws

1. Semantic Neutrality
- ABI is language/runtime neutral.

2. Canonical Determinism
- Same semantic input => ABI-identical structure; same canonical input => same encoded output.

3. Structure-Encoding Separation
- Structure conformance and encoding conformance are separate levels.

4. Canonical Ordering
- Field and sequence ordering is explicit for every ABI-visible structure.

5. Typed Result Preservation
- Success/failure/status families remain typed; no opaque collapse.

6. Evidence Completeness
- Required evidence fields cannot be omitted.

7. EVR Evidence Legality
- EventKind determines evidence shape and is ABI-validated.

8. Closed Status Taxonomies
- Each result family has an explicit closed status set.

9. Execution Exclusion
- Runtime/process/transport details are out of ABI scope.

10. Explicit Versioning
- `abi_version` is explicit on ABI envelopes.

11. Baseline Non-mutation
- ABI maps frozen baseline semantics; it does not redefine them.
