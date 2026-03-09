# DBC / HD-RPC Test Matrix

**Author:** Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
**Status:** Living Document  
**Depends on:** DBC-1.2 · DBC-1.2-FIXTURES · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0

---

## Purpose

This document defines the test surface for the DBC / HD-RPC stack. It maps each test class to the layer boundary it exercises and specifies the input, expected output, and law being proved.

The conformance matrix (`DBC-CONFORMANCE-MATRIX`) maps these test IDs to specific laws. This document defines the tests themselves.

---

## Test Classes

| Class | What it proves |
|---|---|
| Golden | deterministic correctness — same canonical input → same canonical output |
| Negative | reject surface — failures are typed, deterministic, and protocol-correct |
| Extension | schema evolution — new rules do not break existing determinism |
| Adapter | ecosystem projection — SID derivation is isolated, deterministic, non-reversible |
| Integration | full pipeline — SRCall flows correctly through every layer |
| Replay | cross-runtime convergence — federation nodes agree under independent execution |

---

## Golden Tests

Golden tests prove the core invariants of the calculus. They must pass on every runtime implementation, every node, and every CI pipeline.

| ID | Layer | Test | Input | Expected |
|---|---|---|---|---|
| G-01 | DBC | Realization determinism | valid document | RealizedStructure |
| G-02 | DBC | Closure derivation | transitive relation | ClosedStructure with derived edge |
| G-03 | DBC | Normalization canonicalization | alias collapse | NormalForm |
| G-04 | DBC | Projection correctness | relation filter view | ProjectionModel |
| G-05 | DBC | Normalization idempotence | normalize twice | identical NormalForm |
| G-06 | DBC | Closure idempotence | close twice | identical ClosedStructure |
| G-07 | DBC | Confluence | two rewrite orders | same NormalForm |
| G-08 | SR-ABI | Canonical envelope stability | same SRCall bytes | same SRResult bytes |
| G-09 | SR-EABI | Commuting square | encode → exec → decode | equals resolveTo(stage, x) |
| G-10 | NODE | Node idempotence | repeated SRCall | identical SRResult |

---

## Negative Tests

Negative tests verify that the system fails correctly and deterministically. The calculus uses typed rejects rather than exceptions. Every reject must carry an exact `reject_kind` and `reject_code`.

| ID | Reject family | Case | Expected reject |
|---|---|---|---|
| N-01 | RejectRealize | symbol not admitted | `invalid_symbol` |
| N-02 | RejectRealize | wrong clause arity | `invalid_clause` |
| N-03 | RejectClose | contradictory predicates | `contradiction` |
| N-04 | RejectClose | constraint violation | `unsatisfied_constraint` |
| N-05 | RejectNormalize | ambiguous representative | `ambiguous_normal_form` |
| N-06 | RejectNormalize | no canonical ordering | `ordering_failure` |
| N-07 | RejectProject | unknown view | `invalid_view` |
| N-08 | RejectProject | unsupported surface | `surface_incompatibility` |
| N-09 | NODE | unknown `schema_digest` at node boundary | HTTP 422 |
| N-10 | ABI | malformed SRCall at protocol boundary | HTTP 400 |

---

## Extension Tests

Extension tests ensure that new schema rules do not break determinism. The calculus allows schema-defined `closure_rules`, `normalization_rules`, and `projection_constraints`. Each must evolve additively without disturbing existing canonical outputs.

| ID | Layer | Scenario | Test |
|---|---|---|---|
| E-01 | Schema | new closure rule | closure remains deterministic |
| E-02 | Schema | new normalization rule | canonical form unchanged for old inputs |
| E-03 | Schema | projection constraint extension | projections remain read-only |
| E-04 | Federation | new `federation_scope` | SID changes deterministically |
| E-05 | Identity | `derivation_context` added | SID derivation stable |
| E-06 | Node | schema set upgrade | previous artifacts still resolve |
| E-07 | RPC | new stage introduced | earlier stages unaffected |
| E-08 | ABI | new reject code | older runtimes reject gracefully |

---

## Adapter Tests

Adapters derive ecosystem credentials from SIDs. Each adapter must satisfy three laws: domain separation (outputs across adapters differ), determinism (same SID → same output), and non-reversal (credentials cannot be treated as SIDs).

| ID | Adapter | Test | Expected |
|---|---|---|---|
| A-01 | `adapter:evm` | derive address twice | same address |
| A-02 | `adapter:did:key` | DID projection | valid DID document |
| A-03 | `adapter:ipv6` | SID → IPv6 | deterministic address |
| A-04 | `adapter:bip39` | SID → seed | deterministic mnemonic |
| A-05 | domain separation | `adapter:evm` vs `adapter:bip39` | different outputs |
| A-06 | non-reversal | credential → SID | not possible |
| A-07 | federation scope | SID changes | adapter output changes |
| A-08 | node endpoint | `GET /adapter/{label}/{sid}` | matches local derivation |

---

## Integration Tests

Integration tests verify the full HD-RPC pipeline from SRCall through node, calculus, identity, adapter, and projection.

| ID | Flow | Expected |
|---|---|---|
| I-01 | SRCall → NormalForm | canonical NormalForm |
| I-02 | NormalForm → SID | deterministic SID |
| I-03 | SID → descriptor | valid IdentityDescriptor |
| I-04 | SID → `adapter:ipv6` | valid IPv6 routing handle |
| I-05 | IPv6 → `POST /resolve` | correct SRResult |
| I-06 | broker route | descriptor-verified call forwarded verbatim |
| I-07 | federation nodes | identical SRResult on independent nodes |
| I-08 | projection surface | identical ProjectionModel across nodes |

---

## Replay Tests

Replay tests prove cross-implementation convergence. Because the calculus guarantees determinism, any two conforming runtimes must produce identical results for identical inputs — regardless of architecture, language, or execution order.

| ID | Scenario |
|---|---|
| R-01 | same test on two independent runtimes |
| R-02 | replay of historical SRCall logs |
| R-03 | replay across CPU architecture (x86 / ARM) |
| R-04 | replay across language implementations |
| R-05 | federation convergence under independent replay |

---

## CI Layout

```
tests/
  golden/
    G01_realize.json
    G02_close.json
    G03_normalize.json
    G04_project.json
    G05_normalize_idempotence.json
    G06_close_idempotence.json
    G07_confluence.json
    G08_srabi_stability.json
    G09_commuting_square.json
    G10_node_idempotence.json

  negative/
    N01_invalid_symbol.json
    N02_invalid_clause.json
    N03_contradiction.json
    N04_unsatisfied_constraint.json
    N05_ambiguous_normal_form.json
    N06_ordering_failure.json
    N07_invalid_view.json
    N08_surface_incompatibility.json
    N09_unknown_schema.json
    N10_malformed_srcall.json

  extension/
    E01_closure_rule.json
    E02_normalization_upgrade.json
    E03_projection_constraint.json
    E04_federation_scope.json
    E05_derivation_context.json
    E06_schema_upgrade.json
    E07_new_stage.json
    E08_reject_code_extension.json

  adapter/
    A01_evm.json
    A02_did_key.json
    A03_ipv6.json
    A04_bip39.json
    A05_domain_separation.json
    A06_non_reversal.json
    A07_federation_scope_sensitivity.json
    A08_endpoint_faithfulness.json

  integration/
    I01_srcall_to_normalform.json
    I02_normalform_to_sid.json
    I03_sid_to_descriptor.json
    I04_sid_to_ipv6.json
    I05_ipv6_to_resolve.json
    I06_broker_route.json
    I07_federation_convergence.json
    I08_projection_surface.json

  replay/
    R01_cross_runtime.json
    R02_historical_replay.json
    R03_arch_variance.json
    R04_language_variance.json
    R05_federation_replay.json
```

Each test file contains an `SRCall`, an expected `SRResult`, and an assertions block.

---

## Minimal Initial Set

The smallest first deployable conformance set:

```
G-01, G-02, G-03, G-04,
N-01, N-02, N-03, N-04,
G-09,
A-01, A-03, A-05, A-08,
I-01, I-06, I-07
```

This covers core calculus correctness, the primary reject surface, the commuting square, adapter determinism and domain separation, node faithfulness, broker non-invention, and federation convergence.

---

*DBC / HD-RPC Test Matrix · Brian Thorne · bthornemail*
