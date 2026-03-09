# DBC-1.1 Fixtures

**DBC-1.2-FIXTURES · Test Surface**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Normative Test Surface · Depends on: DBC-1.2

---

## Purpose

This file defines the canonical test surface for DBC-1.1. Every conforming implementation must accept all valid fixtures and reject all must-reject fixtures with the exact `reject_kind` and `reject_code` specified.

A fixture set consists of:

- one valid case per stage (`Realized`, `Closed`, `Normalized`, `Projected`)
- one must-reject case per reject family (`RejectRealize`, `RejectClose`, `RejectNormalize`, `RejectProject`)
- one commuting square executable example

---

## Fixture F-01 — Realized (valid)

**Description:** Single clause with two admitted symbols. No closure, no normalization, no projection.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["hasType", "alice", "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "hasType": 2 },
      "closure_rules": [],
      "normalization_rules": [],
      "projection_constraints": [],
      "symbol_admission": ["Person", "alice"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Realized",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_value": {
    "constraints": [],
    "edges": [
      ["hasType", "alice", "Person"]
    ],
    "nodes": ["Person", "alice"],
    "witnesses": []
  },
  "stage":        "Realized",
  "value_digest": "sha256:<value_digest>",
  "value_kind":   "RealizedStructure"
}
```

### Assertions

- `value_kind` is `RealizedStructure`
- `nodes` contains exactly `["Person", "alice"]` in canonical order
- `edges` contains exactly `[["hasType", "alice", "Person"]]`
- No closure edges are present

---

## Fixture F-02 — Closed (valid)

**Description:** Two parent clauses with transitive closure declared. Closure must derive the transitive edge.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["parent", "a", "b"] },
      { "Rel": ["parent", "b", "c"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "parent": 2 },
      "closure_rules": ["Transitive(parent)"],
      "normalization_rules": [],
      "projection_constraints": [],
      "symbol_admission": ["a", "b", "c"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Closed",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_value": {
    "constraints": [],
    "edges": [
      ["parent", "a", "b"],
      ["parent", "a", "c"],
      ["parent", "b", "c"]
    ],
    "nodes": ["a", "b", "c"],
    "witnesses": []
  },
  "stage":        "Closed",
  "value_digest": "sha256:<value_digest>",
  "value_kind":   "ClosedStructure"
}
```

### Assertions

- `value_kind` is `ClosedStructure`
- `edges` contains exactly three edges: the two input edges plus the derived transitive edge `["parent", "a", "c"]`
- Edges are in canonical lexicographic order
- Closure is idempotent: running closure again on this result must produce the same result

---

## Fixture F-03 — Normalized (valid)

**Description:** Alias between `bob` and `alice`, with `hasType` on `bob`. Normalization must collapse `bob` to `alice` (canonical representative) and rewrite the `hasType` edge.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["alias",   "bob",  "alice"] },
      { "Rel": ["hasType", "bob",  "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "alias": 2, "hasType": 2 },
      "closure_rules": [
        "Symmetric(alias)",
        "AliasPropagation(hasType)"
      ],
      "normalization_rules": [
        "CanonicalRepresentative",
        "RepresentativeSubstitution",
        "EdgeSorting",
        "DuplicateElimination"
      ],
      "projection_constraints": [],
      "symbol_admission": ["Person", "alice", "bob"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Normalized",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_value": {
    "constraints": [],
    "edges": [
      ["alias",   "alice", "alice"],
      ["hasType", "alice", "Person"]
    ],
    "nodes": ["Person", "alice"],
    "witnesses": []
  },
  "stage":        "Normalized",
  "value_digest": "sha256:<value_digest>",
  "value_kind":   "NormalForm"
}
```

### Assertions

- `value_kind` is `NormalForm`
- `bob` does not appear in `nodes` or any edge
- `alice` is the canonical representative
- `alias(alice, alice)` is present (self-alias of the representative)
- `hasType(alice, Person)` has been rewritten from `hasType(bob, Person)`
- Normalization is idempotent: running normalize again must produce the same result
- Schema preservation: no node in the result is outside `["Person", "alice", "bob"]` (and `bob` has been collapsed, not introduced fresh)

---

## Fixture F-04 — Projected (valid)

**Description:** Same alias/hasType input as F-03, but with a view that filters to `hasType` edges only. The `alias` edge must not appear in the projection result.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["alias",   "bob",  "alice"] },
      { "Rel": ["hasType", "bob",  "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "alias": 2, "hasType": 2 },
      "closure_rules": [
        "Symmetric(alias)",
        "AliasPropagation(hasType)"
      ],
      "normalization_rules": [
        "CanonicalRepresentative",
        "RepresentativeSubstitution",
        "EdgeSorting",
        "DuplicateElimination"
      ],
      "projection_constraints": ["relations"],
      "symbol_admission": ["Person", "alice", "bob"],
      "term_formation": {}
    },
    "v": { "relations": ["hasType"] }
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Projected",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_value": {
    "edges": [
      ["hasType", "alice", "Person"]
    ]
  },
  "stage":        "Projected",
  "value_digest": "sha256:<value_digest>",
  "value_kind":   "ProjectionModel"
}
```

### Assertions

- `value_kind` is `ProjectionModel` — never `SurfaceArtifact`
- Only `hasType` edges appear; `alias` edges are absent
- The projection result is a semantic view of the normal form, not a mutation and not a surface encoding
- Running the same projection on the same normal form must produce the same result
- `emit` has not been called; `canonical_value` is a projection model, not rendered bytes

---

## Fixture F-R1 — RejectRealize (must-reject)

**Description:** Document contains symbol `charlie`, which is not in `symbol_admission`. Realization must reject.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["hasType", "charlie", "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "hasType": 2 },
      "closure_rules": [],
      "normalization_rules": [],
      "projection_constraints": [],
      "symbol_admission": ["Person", "alice"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Realized",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_evidence": {
    "symbol": "charlie"
  },
  "reject_code": "invalid_symbol",
  "reject_kind": "RejectRealize",
  "stage":       "Realized"
}
```

### Assertions

- `reject_kind` is exactly `RejectRealize`
- `reject_code` is exactly `invalid_symbol`
- `canonical_evidence.symbol` names the offending symbol
- No partial `canonical_value` is returned

---

## Fixture F-R2 — RejectClose (must-reject)

**Description:** Document asserts both `p(a)` and `not_p(a)`. Closure detects contradiction and must reject.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["p",     "a"] },
      { "Rel": ["not_p", "a"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "not_p": 1, "p": 1 },
      "closure_rules": ["Contradiction(p,not_p)"],
      "normalization_rules": [],
      "projection_constraints": [],
      "symbol_admission": ["a"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Closed",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_evidence": {
    "edges": [
      ["not_p", "a"],
      ["p",     "a"]
    ]
  },
  "reject_code": "contradiction",
  "reject_kind": "RejectClose",
  "stage":       "Closed"
}
```

### Assertions

- `reject_kind` is exactly `RejectClose`
- `reject_code` is exactly `contradiction`
- `canonical_evidence.edges` names both contradicting edges
- Realization of this input would succeed; only closure fails

---

## Fixture F-R3 — RejectNormalize (must-reject)

**Description:** Two clauses assign conflicting canonical types to the same node: `hasType(alice, Person)` and `hasType(alice, Animal)`. The schema declares `hasType` as a functional relation (at most one type per node). Normalization detects the functional violation and must reject.

This fixture survives any total canonical ordering over admitted symbols. It does not rely on tie-breaking failure; it relies on a structural constraint that no ordering can resolve.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["hasType", "alice", "Animal"] },
      { "Rel": ["hasType", "alice", "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "hasType": 2 },
      "closure_rules": [],
      "normalization_rules": [
        "FunctionalRelation(hasType)"
      ],
      "projection_constraints": [],
      "symbol_admission": ["Animal", "Person", "alice"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Normalized",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_evidence": {
    "relation": "hasType",
    "node":     "alice",
    "values":   ["Animal", "Person"]
  },
  "reject_code": "ambiguous_normal_form",
  "reject_kind": "RejectNormalize",
  "stage":       "Normalized"
}
```

### Assertions

- `reject_kind` is exactly `RejectNormalize`
- `reject_code` is exactly `ambiguous_normal_form`
- `canonical_evidence` identifies the relation, the node, and the conflicting values
- This reject occurs regardless of the canonical ordering over `["Animal", "Person", "alice"]`
- Realization and closure of this input succeed; only normalization fails
- The reject is stable under any total lexicographic order over admitted symbols

---

## Fixture F-R4 — RejectProject (must-reject)

**Description:** View spec references `unknown_view`, which is not declared in `projection_constraints`. Projection must reject as `invalid_view`.

### SRCall

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["hasType", "alice", "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "hasType": 2 },
      "closure_rules": [],
      "normalization_rules": [],
      "projection_constraints": ["relations"],
      "symbol_admission": ["Person", "alice"],
      "term_formation": {}
    },
    "v": { "unknown_view": true }
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Projected",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected SRResult

```json
{
  "canonical_evidence": {
    "view": { "unknown_view": true }
  },
  "reject_code": "invalid_view",
  "reject_kind": "RejectProject",
  "stage":       "Projected"
}
```

### Assertions

- `reject_kind` is exactly `RejectProject`
- `reject_code` is exactly `invalid_view`
- `canonical_evidence.view` reproduces the offending view spec
- Realization, closure, and normalization of this input succeed; only projection fails

---

## Fixture F-CS — Commuting Square (executable)

**Description:** Verifies that executing a canonical SRCall through an EABI and decoding the result is equal to calling `resolveTo` directly. Uses the `Normalized` stage with alias collapse.

### The law being tested

```
decode_Normalized(exec_Normalized(encode_in(x))) = resolveTo(Normalized, x)
```

### Input `x`

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["alias",   "bob",  "alice"] },
      { "Rel": ["hasType", "bob",  "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "alias": 2, "hasType": 2 },
      "closure_rules": [
        "Symmetric(alias)",
        "AliasPropagation(hasType)"
      ],
      "normalization_rules": [
        "CanonicalRepresentative",
        "RepresentativeSubstitution",
        "EdgeSorting",
        "DuplicateElimination"
      ],
      "projection_constraints": [],
      "symbol_admission": ["Person", "alice", "bob"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest": "sha256:<document_digest>",
  "null_digest":     "sha256:<null_digest>",
  "schema_digest":   "sha256:<schema_digest>",
  "target_stage":    "Normalized",
  "view_digest":     "sha256:<view_digest>"
}
```

### Expected canonical result (both paths must produce this)

```json
{
  "canonical_value": {
    "constraints": [],
    "edges": [
      ["alias",   "alice", "alice"],
      ["hasType", "alice", "Person"]
    ],
    "nodes": ["Person", "alice"],
    "witnesses": []
  },
  "stage":        "Normalized",
  "value_digest": "sha256:<value_digest>",
  "value_kind":   "NormalForm"
}
```

### Commuting square test procedure

1. Encode `x` to canonical bytes: `bytes_in = encode_in(x)`
2. Execute via EABI: `bytes_out = exec_Normalized(bytes_in)`
3. Decode result: `result_eabi = decode_Normalized(bytes_out)`
4. Call resolver directly: `result_direct = resolveTo(Normalized, x)`
5. Assert: `result_eabi = result_direct`

### Assertions

- Both paths produce `value_kind = NormalForm`
- Both paths produce identical `canonical_value`
- Both paths produce identical `value_digest`
- Neither path produces a reject
- The test is deterministic: running it twice produces the same outcome

---

## Fixture Index

| ID | Stage | Kind | Description |
|---|---|---|---|
| F-01 | `Realized` | valid | Single hasType clause |
| F-02 | `Closed` | valid | Transitive parent closure |
| F-03 | `Normalized` | valid | Alias collapse to canonical representative |
| F-04 | `Projected` | valid | Relation-filtered projection |
| F-R1 | `Realized` | must-reject | Unadmitted symbol `charlie` |
| F-R2 | `Closed` | must-reject | Contradiction `p(a)` and `not_p(a)` |
| F-R3 | `Normalized` | must-reject | Functional violation: conflicting types for same node |
| F-R4 | `Projected` | must-reject | Unknown view spec |
| F-CS | `Normalized` | commuting square | EABI faithfulness check |

---

## Conformance Rule

An implementation is DBC-1.2 conformant when:

- F-01 through F-04 all produce the exact `canonical_value` shown
- F-R1 through F-R4 all produce the exact `reject_kind` and `reject_code` shown
- F-CS satisfies the commuting square equality
- All `value_kind` fields match the stage exactly as specified in §3.4 of DBC-1.2
- `value_kind` for `Projected` results is always `ProjectionModel`; `SurfaceArtifact` is never present in an SRResult
- F-R3 produces `RejectNormalize` / `ambiguous_normal_form` regardless of the implementation's canonical ordering over admitted symbols

---

*DBC-1.2-FIXTURES · Deterministic Blackboard Calculus · Brian Thorne · bthornemail*
