# EVR Module Fixture Matrix (Track B)

This matrix defines required EVR fixture coverage before runtime event wiring.

## Golden fixtures

### E-G01 Resolve success event

- `resolution.resolve_succeeded`
- Evidence includes stage, value kind, canonical refs, and optional SID when present.

### E-G02 Resolve reject event

- `resolution.resolve_rejected`
- Evidence includes reject kind/code/evidence digest and causal call reference.

### E-G03 Descriptor lookup success event

- `descriptor.lookup_succeeded`
- Evidence includes SID, descriptor digest/ref, and lookup scope.

### E-G04 Capability verification success event

- `capability.verify_succeeded`
- Evidence includes verification status `verified`, chain digest, and subject/actor linkage.

### E-G05 Capability verification failure event

- `capability.verify_rejected`
- Evidence includes deterministic reject status and evidence payload digest.

### E-G06 Adapter derivation success event

- `adapter.derive_succeeded`
- Evidence includes adapter label, SID, and projection authority flag.

### E-G07 Adapter derivation failure event

- `adapter.derive_rejected`
- Evidence includes reject code and requested adapter label.

### E-G08 Route success event

- `route.resolve_succeeded`
- Evidence includes SID and route target.

### E-G09 Route failure event

- `route.resolve_failed`
- Evidence includes SID and deterministic route failure code.

### E-G10 Hub projection event

- `hub.inspect_emitted`
- Evidence includes pane identifier and upstream operation correlation.

## Negative fixtures

### E-N01 Malformed envelope

- Missing required top-level fields (`family`, `kind`, `source_layer`, `evidence`).

### E-N02 Wrong evidence shape for kind

- Legal `EventKind`, illegal evidence keys/types.

### E-N03 Missing causal field

- Outcome event missing required causal reference.

### E-N04 Invalid family/kind pairing

- `family` and `kind` do not form legal inventory pair.

### E-N05 Invalid timestamp/sequence

- Bad timestamp encoding or invalid/non-monotonic sequence in scoped stream.

### E-N06 Impossible source/layer combination

- Event claims source layer not permitted for declared kind.

## Determinism fixtures

### E-D01 Same operation -> same event kind/evidence schema

- Repeated identical operation emits same `family`, `kind`, and evidence shape.

### E-D02 Equivalent failing operation -> same reject event schema

- Equivalent failures map to same reject kind and evidence schema.

### E-D03 Stable ordering rules

- Repeated runs preserve ordering rules (canonical sort/append order as defined).

### E-D04 Stable timeline projection

- Event timeline projection remains stable for equivalent operation traces.

## Implemented vs reserved scope

Implemented fixture obligations in this module:

- `resolution.*`, `descriptor.*`, `capability.*`, `adapter.*`, `route.*`, `hub.*`

Reserved/deferred fixture obligations:

- `federation.*`, `device.*`
