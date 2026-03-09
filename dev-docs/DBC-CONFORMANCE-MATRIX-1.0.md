# DBC-CONFORMANCE-MATRIX

**Author:** Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
**Status:** Normative Conformance Surface · Living Document  
**Audience:** maintainers · implementers · CI systems · federation operators  
**Depends on:** DBC-1.2 · DBC-1.2-FIXTURES · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0 · DBC-TEST-MATRIX

---

## Purpose

This document maps laws to test classes to specific fixtures to expected evidence.

It answers four questions for every law in the stack:

- which law is being tested
- where it is defined
- which test ID or fixture proves it
- what constitutes pass / fail evidence

This document is a projection of the stack law inventory into executable test coverage. The test definitions live in `DBC-TEST-MATRIX`. The normative fixture surface lives in `DBC-1.2-FIXTURES`. The law inventory lives in `DBC-INDEX`.

---

## Conformance Layers

| Layer | Role | Primary source |
|---|---|---|
| DBC calculus | stage law · rewrite law · projection law | DBC-1.2 |
| DBC fixtures | canonical stage and reject cases | DBC-1.2-FIXTURES |
| Identity | SID · capability · adapter · descriptor laws | DBC-IDL-1.2 |
| Node | HTTP endpoint faithfulness | DBC-NODE-1.0 |
| HD-RPC | federation · routing · semantic addressing | HD-RPC-1.0 |

---

## Test Classes

| Class | Meaning |
|---|---|
| Golden | valid canonical success path |
| Negative | must-reject / protocol-fail path |
| Extension | additive evolution without law break |
| Adapter | ecosystem derivation and isolation |
| Integration | cross-layer end-to-end behavior |
| Replay | cross-runtime determinism and convergence |

---

## 4.1 DBC Core Laws

| Law | Source | Test class | Test ID | Pass evidence |
|---|---|---|---|---|
| Stage Soundness | DBC §1.5 | Golden | G-01, G-02, G-03, G-04 | `value_kind` exactly matches stage codomain |
| Failure Monotonicity | DBC §1.6 | Negative | N-01..N-08 | earlier-stage failure prevents later-stage success |
| Closure Idempotence | DBC §2.5 | Golden | G-06 | rerun of `close` yields identical canonical value |
| Normalization Idempotence | DBC §2.6 | Golden | G-05 | rerun of `normalize` yields identical canonical value |
| Confluence | DBC §2.7 | Golden / Replay | G-07, R-01 | all rewrite orders converge to same normal form |
| Schema Preservation | DBC §2.5 / §2.6 | Golden / Negative | G-03, E-01 | no unadmitted nodes or edges introduced |
| Projection Read-Only | DBC projection law | Golden | G-04 | projection omits forbidden relations without mutating normal form |
| Operator/Emit Separation | DBC projection section | Golden | G-04 | `value_kind = ProjectionModel`, not `SurfaceArtifact` |
| Commuting Square | DBC / SR-EABI law | Golden / Replay | G-09 | `decode(exec(encode(x))) = resolveTo(stage, x)` |

---

## 4.2 Reject Family Laws

| Law / Reject family | Source | Test class | Test ID | Pass evidence |
|---|---|---|---|---|
| RejectRealize correctness | DBC-1.2-FIXTURES | Negative | N-01, N-02 | exact `reject_kind` and `reject_code` |
| RejectClose correctness | DBC-1.2-FIXTURES | Negative | N-03, N-04 | contradiction or unsatisfied constraint reported canonically |
| RejectNormalize correctness | DBC-1.2-FIXTURES | Negative | N-05, N-06 | normalization failure reported canonically |
| RejectProject correctness | DBC-1.2-FIXTURES | Negative | N-07, N-08 | invalid view / incompatibility reported canonically |
| Protocol error vs SR reject split | DBC-NODE-1.0 `/resolve` semantics | Negative | N-09, N-10 | HTTP error only when request never reaches calculus |

---

## 4.3 Identity Layer Laws

| Law | Source | Test class | Test ID | Pass evidence |
|---|---|---|---|---|
| SID Determinism | IDL §1.3 | Golden / Replay | I-02, R-01 | same normal form + same schema + same scope = same SID |
| Canonical Dependency | IDL §1.3 | Negative | N-09 | SID derivation from non-normal form rejected |
| Schema Pinning | IDL §1.3 | Extension | E-04 | same structure under different schema gives different SID |
| Meaning Primacy | IDL §1.3 / §2 | Golden | I-02 | identity stable independent of key material |
| Canonical Encoding | IDL §1.7 | Golden / Replay | G-08 | same logical inputs give same encoded bytes |
| Namespace Law | IDL §1.8 | Negative | N-10 | bare digest rejected as textual SID |
| Capability Delegation | IDL §2.3 | Integration | I-06 | valid governor chain accepted |
| No Self-Authorization | IDL §2.3 | Negative | N-09 | actor may not self-authorize |
| Epoch-Boundedness | IDL §2.3 | Negative / Extension | E-05 | expired grant rejected |
| Non-Confusion | IDL §2.3 | Negative | N-04 | grant for SID(σ1) not valid for SID(σ2) |

---

## 4.4 Adapter Laws

| Law | Source | Test class | Test ID | Pass evidence |
|---|---|---|---|---|
| Adapter Isolation | IDL §3.2 | Adapter | A-05 | domain-separated outputs differ |
| Non-Reversal | IDL §3.2 | Adapter | A-06 | credential cannot be treated as SID |
| Adapter Determinism | IDL §3.2 | Adapter | A-01, A-02, A-03, A-04 | repeated derivation gives identical output |
| IPv6 Non-Authority | HD-RPC-1.0 §9 | Adapter / Negative | A-03, I-04 | derived IPv6 used only as routing handle |
| Adapter endpoint faithfulness | DBC-NODE-1.0 `/adapter/{label}/{sid}` | Adapter | A-08 | endpoint result matches local derivation |

---

## 4.5 Node Laws

| Law | Source | Test class | Test ID | Pass evidence |
|---|---|---|---|---|
| Calculus Faithfulness | DBC-NODE-1.0 `/resolve` | Integration | I-01 | node result equals local `resolveTo` |
| Artifact Immutability | DBC-NODE-1.0 artifact store | Golden / Replay | G-10, R-02 | stored artifact digest never changes |
| Descriptor Non-Authority | DBC-NODE-1.0 `/sid/{digest}` | Negative | N-09 | descriptor disagreement with artifact store is forbidden |
| Descriptor Read-Only | DBC-NODE-1.0 descriptor serving | Integration | I-03 | served descriptor is canonical and stable |
| Endpoint Minimality | DBC-NODE-1.0 four endpoints | Golden | G-10 | conforming node exposes required four endpoints |
| Idempotent `/resolve` | DBC-NODE-1.0 §2.7 | Replay | G-10, R-02 | identical SRCall yields identical SRResult |

---

## 4.6 HD-RPC Laws

| Law | Source | Test class | Test ID | Pass evidence |
|---|---|---|---|---|
| Semantic Address | HD-RPC-1.0 §9 | Integration | I-01, I-02 | same meaning derives same SID and resolves same result |
| HD Derivation | HD-RPC-1.0 §9 | Golden / Replay | I-02, R-01 | nested semantic structures produce nested SIDs deterministically |
| Call Determinism | HD-RPC-1.0 §9 | Replay | R-01, R-05 | same SRCall → same SRResult on any conforming node |
| Transport Neutrality | HD-RPC-1.0 §9 | Integration | I-05 | SID and SRResult invariant across transports |
| IPv6 Non-Authority | HD-RPC-1.0 §9 | Adapter / Negative | A-03, I-04 | IPv6 address is routing handle only |
| Namespace Without Registrar | HD-RPC-1.0 §9 | Integration | I-02, I-03 | SID derivable from schema and descriptor without registry |
| Federation Convergence | HD-RPC-1.0 §9 | Replay | I-07, R-05 | conforming federation nodes produce identical SRResult |
| Physical Uniformity | HD-RPC-1.0 §9 | Integration | I-02 | device descriptors participate in same SID namespace as software |
| Broker No Semantic Invention | HD-RPC-1.0 deployment profiles | Negative / Integration | I-06 | broker forwards SRCall and SRResult verbatim |
| Semantic Routing | HD-RPC-1.0 semantic routing | Adapter / Integration | I-04, I-05 | SID-derived IPv6 reaches correct node |

---

## Evidence Rules

A test passes only when it yields:

- exact canonical value or exact canonical reject
- exact `stage`
- exact `value_kind` or `reject_kind`
- exact digest agreement where required
- exact commuting-square equality where required

Approximate equivalence is not sufficient unless a specific law explicitly permits it.

---

## Acceptance Rule

A stack implementation is conformant only if:

- all required golden tests pass
- all required negative tests reject exactly as specified
- all adapter laws hold
- all replay tests converge
- all node and HD-RPC integration checks preserve canonical faithfulness

---

## Minimal Initial Set

The smallest first deployable conformance set:

```
G-01, G-02, G-03, G-04,
N-01, N-02, N-03, N-04,
G-09,
A-01, A-03, A-05, A-08,
I-01, I-06, I-07,
R-01, R-05
```

This covers core calculus correctness, the primary reject surface, the commuting square, adapter determinism and domain separation, node faithfulness, broker non-invention, federation convergence, and cross-runtime replay.

---

*DBC-CONFORMANCE-MATRIX · Brian Thorne · bthornemail*
