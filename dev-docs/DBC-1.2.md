# Deterministic Blackboard Calculus

**DBC-1.1 · Specification**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Draft Specification · Audience: maintainers, implementers, agents

---

## Abstract

The Deterministic Blackboard Calculus (DBC) defines a typed, stage-indexed computation model for shared symbolic state. It specifies how agents may write into a common blackboard, how relational consequences are derived, how canonical form is achieved, and how the resulting state is safely projected to observation surfaces.

The calculus is organized around a single principle:

> **The requested stage determines the codomain.**

All agents, surfaces, and runtimes must conform to this law. The blackboard is not a mutable data store. It is a typed relational structure that evolves through lawful rewrite stages toward a canonical normal form from which all valid views are derived by projection.

---

## 0. Canonical Vocabulary

The following terms have fixed meanings throughout this specification.

| Term | Meaning |
|---|---|
| blackboard | Shared canonical symbolic state |
| document | Input symbolic description; a finite sequence of clauses |
| structure | Realized relational state; a set of typed nodes and edges |
| realized structure | Structure admitted under schema; not yet closed |
| closed structure | Structure plus all admissible relational consequences |
| normal form | Canonical blackboard state; unique representative per equivalence class |
| projection | Lawful, read-only view transformation from canonical state |
| projection model | Semantic view selection from normal form |
| surface artifact | Emitted concrete representation (JSON, SVG, scene, etc.) |
| surface | Concrete rendered observer; receives projection model or surface artifact |
| schema | The admissibility law governing symbols, terms, clauses, closure, and normalization |
| agent | Any party that submits documents to the blackboard |
| stage | One of: `Realized`, `Closed`, `Normalized`, `Projected` |
| reject | A typed failure value carrying evidence; never an exception |

---

## Document Structure

| Layer | Role |
|---|---|
| Layer 1 — Stage Law | The stage-indexed resolution family and its type guarantees |
| Layer 2 — Rewrite Engine | The realization, closure, and normalization rules |
| Layer 3 — Wire Protocol | SR-ABI canonical JSON envelopes and must-reject rules |
| Observation — Projection | Runtime projection interface, operator, and surface contract |

---

## 1. Stage-Indexed Resolution

### 1.1 Core Type Family

Resolution is not a single untyped function. It is a family of functions indexed by stage:

```
resolveTo :
  (s : Stage) →
  Input →
  Either (RejectUpTo s) (ValueAt s)
```

| Component | Meaning |
|---|---|
| `Stage` | Determines the target layer of resolution |
| `ValueAt s` | Determines the success codomain |
| `RejectUpTo s` | Determines the allowable failure space |

### 1.2 Stage Order

The resolution pipeline is strictly ordered. Later stages depend on earlier stages.

| Order | Stage | Meaning |
|---|---|---|
| 0 | `Realized` | Symbolic inputs admitted into typed structure |
| 1 | `Closed` | Relational consequences derived |
| 2 | `Normalized` | Canonical form obtained |
| 3 | `Projected` | Surface-specific projection produced |

```
Realized < Closed < Normalized < Projected
```

### 1.3 Value Family

The success type is determined by stage. `Realized` and `Closed` return distinct structural subtypes; they are not interchangeable.

```
ValueAt : Stage → Type

ValueAt Realized   = RealizedStructure
ValueAt Closed     = ClosedStructure
ValueAt Normalized = NormalForm
ValueAt Projected  = ProjectionResult
```

where:

```
RealizedStructure ⊆ Structure   -- admitted, not yet closed
ClosedStructure   ⊆ Structure   -- admitted and consequence-complete
NormalForm        ⊆ ClosedStructure   -- canonical representative selected
```

A `RealizedStructure` must not be passed to any operation that expects a `ClosedStructure`. They carry different semantic guarantees.

### 1.4 Reject Family

The failure type is also determined by stage and grows monotonically:

```
RejectUpTo : Stage → Type

RejectUpTo Realized   = RejectRealize
RejectUpTo Closed     = RejectRealize ⊎ RejectClose
RejectUpTo Normalized = RejectRealize ⊎ RejectClose ⊎ RejectNormalize
RejectUpTo Projected  = RejectRealize ⊎ RejectClose ⊎ RejectNormalize ⊎ RejectProject
```

> **Law:** Later stages may fail for more reasons, but never for fewer.

### 1.5 Stage Soundness

For every stage `s`, the following must hold:

```
resolveTo(s, i) = Right v  ⇒  v : ValueAt s
```

If resolution succeeds, the result inhabits exactly the codomain determined by the stage. This is the primary type-theoretic guarantee.

### 1.6 Failure Monotonicity

> **Law:** If `resolveTo(s, i)` fails, then any later stage `t ≥ s` also fails unless the failure is repaired upstream.

- Failed realization blocks all later stages
- Failed closure blocks normalization and projection
- Failed normalization blocks projection

### 1.7 The Full Resolver as Terminal Member

The ordinary end-to-end blackboard resolver is simply the `Projected` member of the stage family:

```
resolveRuntimeProjection(i)
  = resolveTo(Projected, i)
```

The familiar projection function is not special. It is the maximal stage.

### 1.8 Determination Principle

A result is well-defined only if at least one of the following determines the codomain:

| Source | Role |
|---|---|
| Explicit stage | Caller specifies the target stage directly |
| Query form | Target inferred from the shape of the query |
| Schema law | Admissible target constrained by the schema |

> **Constraint:** If multiple sources determine the codomain, they must agree.

---

## 2. Pure Rewrite System

### 2.1 Object Language

```
Symbol   ::= atomic identifier
Term     ::= Symbol | Apply(Symbol, Term*)
Clause   ::= Rel(RelName, Term*)
Document ::= finite sequence of Clause
```

> **Document order:** Document order is admissible as input, but canonical meaning is determined only after realization and normalization. Clause order must not affect the canonical value of any stage result. Implementations may accept ordered input but must produce order-independent canonical output.

### 2.2 Structural Domain

```
Node        ::= canonicalized term node
Edge        ::= typed relation between nodes
Structure   ::= (Nodes, Edges, Constraints, Witnesses)
NormalForm  ::= canonical Structure
```

### 2.3 Schema Domain

```
Schema ::= (
  symbol_admission,
  term_formation,
  clause_formation,
  closure_rules,
  normalization_rules,
  projection_constraints
)
```

### 2.4 Realization Rules

Realization constructs typed structure from symbolic input.

#### Symbol Admission (R-SYMBOL)

```
sym ∈ AdmitSymbols(σ)
———————————————————————
σ ⊢ sym ⇓s Node(sym)

sym ∉ AdmitSymbols(σ)
———————————————————————
σ ⊢ sym ⇓s Reject invalid_symbol
```

#### Clause Realization (R-CLAUSE)

```
rel ∈ AdmitRelations(σ)
σ ⊢ t1 ⇓t n1  …  σ ⊢ tk ⇓t nk
Arityσ(rel) = k
————————————————————————————————
σ ⊢ Rel(rel, [t1…tk]) ⇓c Edge(rel; n1…nk)
```

#### Document Fold

```
σ ⊢ [] ⇓r EmptyRealizedStructure

σ ⊢ c ⇓c e    σ ⊢ ds ⇓r S
————————————————————————————
σ ⊢ (c :: ds) ⇓r InsertEdge(e, S)
```

> **Invariant:** Insertion must intern equal nodes, preserve stable ordering, accumulate witnesses, and avoid duplicate edges by canonical key.

### 2.5 Closure Rules

Closure derives all admissible relational consequences. It computes the least fixed point under schema rules.

#### Transitive Closure (C-TRANS)

```
Transitiveσ(rel)
Edge(rel; a,b) ∈ S
Edge(rel; b,c) ∈ S
————————————————————
σ ⊢ S ⇓cl AddEdge(rel; a,c, S)
```

#### Symmetric Closure (C-SYM)

```
Symmetricσ(rel)
Edge(rel; a,b) ∈ S
————————————————————
σ ⊢ S ⇓cl AddEdge(rel; b,a, S)
```

#### Contradiction Detection (C-CONTRA)

```
Edge(p; a) ∈ S
Edge(not_p; a) ∈ S
————————————————————
σ ⊢ S ⇓cl Reject contradiction
```

#### Closure Fixed Point

```
Closedσ(S)  iff  StepCloseσ(S) = S

close(σ, S) = μX. StepCloseσ(X)  with seed S
```

> **Law:** `close(σ, close(σ, S)) = close(σ, S)`

#### Schema Preservation at Closure

> **Schema Preservation Law:** If `σ ⊢ S ⇓cl S'`, then every node and edge newly introduced in `S'` is admitted by `σ`. Closure rules may not introduce symbols outside schema-declared constructors.

Formally:

```
σ ⊢ S ⇓cl S'
⇒  Nodes(S') \ Nodes(S)  ⊆  AdmitNodes(σ)
∧  Edges(S') \ Edges(S)  ⊆  AdmitEdges(σ)
```

### 2.6 Normalization Rules

Normalization collapses equivalent structures into a single canonical representative.

#### Canonical Representative (N-REP)

```
a ~σ b
CanonOrder(a) < CanonOrder(b)
————————————————————————————
rep([a,b]) = a
```

#### Representative Substitution (N-SUBST)

```
rep([a]) = a*
Edge(r; … a …) ∈ S
——————————————————
rewrite Edge(r; … a …) ↦ Edge(r; … a* …)
```

#### Normal Form Conditions

A structure is in normal form when:

- All aliases resolved to canonical representatives
- All edges sorted by canonical lexicographic order
- No duplicate edges remain
- All witness sets canonical
- No normalization rule applies

> **Law:** `normalize(σ, normalize(σ, S)) = normalize(σ, S)`

#### Schema Preservation at Normalization

> **Schema Preservation Law:** If `σ ⊢ S ⇓nf N`, then every node and edge in `N` is admitted by `σ`. Normalization may collapse nodes but must not introduce unadmitted ones.

Formally:

```
σ ⊢ S ⇓nf N
⇒  Nodes(N) ⊆ AdmitNodes(σ)
∧  Edges(N) ⊆ AdmitEdges(σ)
```

### 2.7 Confluence

> **Confluence Law (rewrite-theoretic form):** If `S ↠ N1` and `S ↠ N2` and both `N1`, `N2` are normal forms under `σ`, then `N1 = N2`.

This guarantees uniqueness of normal form. Every input structure has at most one canonical representative under a given schema.

> **Confluence Law (distributed form):** Any two valid normal forms that share a common prefix input must agree on all shared relational consequences.

The distributed form is the blackboard interpretation of the rewrite-theoretic form. It is the condition that makes concurrent agent writes coherent and provides a canonical merge procedure for conflicting intermediate states.

### 2.8 Meta-Properties

| Property | Statement |
|---|---|
| Determinism | Each stage relation yields a unique result |
| Closure idempotence | `close(σ, close(σ, S)) = close(σ, S)` |
| Normalization idempotence | `normalize(σ, normalize(σ, S)) = normalize(σ, S)` |
| Confluence | `S ↠ N1` and `S ↠ N2` both normal ⇒ `N1 = N2` |
| Semantic preservation | `Sem(normalize(σ, S)) = Sem(S)` |
| Schema preservation | Closure and normalization introduce no unadmitted nodes or edges |
| Projection read-only | `project(v, N)` is a view of `N`, not a mutation |

### 2.9 Full Staged Resolver

```
resolveTo(Realized,   i) = realize(i.null, i.sigma, i.d)

resolveTo(Closed,     i) = realize(...)
                           >>= close(i.sigma)

resolveTo(Normalized, i) = realize(...)
                           >>= close(i.sigma)
                           >>= normalize(i.sigma)

resolveTo(Projected,  i) = realize(...)
                           >>= close(i.sigma)
                           >>= normalize(i.sigma)
                           >>= project(i.v)        -- returns ProjectionModel
```

Surface emission is outside `resolveTo`:

```
emit(surface_class, project_result)              -- returns SurfaceArtifact
```

`resolveTo(Projected, i)` never returns a `SurfaceArtifact`. It returns a `ProjectionModel`.

---

## 3. SR-ABI Wire Protocol

### 3.1 Purpose

The SR-ABI (Stage Resolution ABI) defines canonical artifact encodings for `Stage`, `Input`, `ValueAt(s)`, and `RejectUpTo(s)`, independent of host or runtime. It is the interoperability contract for every agent that writes to or reads from the blackboard.

### 3.2 ABI Laws

- Strict canonical key ordering in all JSON objects
- Canonical byte encoding for all values
- Deterministic digest computation
- Stage-indexed success codomain
- Stage-indexed reject family
- Same canonical input bytes → same canonical output bytes, or same canonical reject bytes

### 3.3 Canonical SRCall Envelope

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [],
    "n": null,
    "sigma": {
      "clause_formation": {},
      "closure_rules": [],
      "normalization_rules": [],
      "projection_constraints": [],
      "symbol_admission": [],
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

### 3.4 Canonical SRResult Envelopes

#### Success

```json
{
  "canonical_value": {},
  "stage":        "Realized",
  "value_digest": "sha256:<value_digest>",
  "value_kind":   "RealizedStructure"
}
```

`value_kind` must match the stage exactly. The SR-ABI carries the operator result, not the surface artifact. `SurfaceArtifact` is produced by `emit` after the ABI boundary and is never a valid `value_kind`.

| Stage | `value_kind` |
|---|---|
| `Realized` | `RealizedStructure` |
| `Closed` | `ClosedStructure` |
| `Normalized` | `NormalForm` |
| `Projected` | `ProjectionModel` |

#### Reject

```json
{
  "canonical_evidence": {},
  "reject_code": "invalid_symbol",
  "reject_kind": "RejectRealize",
  "stage":       "Realized"
}
```

### 3.5 Reject Code Vocabulary

#### RejectRealize

| Code | Meaning |
|---|---|
| `invalid_symbol` | Symbol not admitted by schema |
| `invalid_term` | Term construction invalid |
| `invalid_clause` | Clause shape invalid |
| `schema_violation` | Document violates schema admission |

#### RejectClose

| Code | Meaning |
|---|---|
| `contradiction` | Derived relations are inconsistent |
| `unsatisfied_constraint` | Required relation cannot be established |
| `alias_conflict` | Closure produces incompatible identifications |

#### RejectNormalize

| Code | Meaning |
|---|---|
| `non_normalizable` | No canonical normal form exists |
| `ambiguous_normal_form` | Competing canonicalizations remain |
| `ordering_failure` | Canonical ordering cannot be established |

#### RejectProject

| Code | Meaning |
|---|---|
| `invalid_view` | Requested view is not admitted |
| `projection_constraint_failure` | View cannot preserve required invariants |
| `surface_incompatibility` | Surface cannot represent the canonical model |

---

## 4. Projection Layer

### 4.1 Stack Position

```
protocol law
    ↓
governed runtime
    ↓
runtime projection interface      [observation boundary]
    ↓
projection operator π             [lawful view transform]
    ↓
projection surface contract       [no-backflow GUI law]
    ↓
gui surfaces
```

### 4.2 Runtime Projection Interface

The RPI is the stable boundary between deterministic runtime truth and projection systems. It exposes runtime artifacts without granting mutation authority.

| Channel | Description |
|---|---|
| `event-stream` | Canonical runtime event log |
| `state-snapshot` | Deterministic world snapshot |
| `artifact-reference` | Canonical artifact identifiers |
| `verification-channel` | Runtime verification results |
| `projection-metadata` | View hints safe for projection |

> **Law:** Same runtime state → identical interface output. Projection surfaces must not introduce nondeterminism.

**Authority:**

| Capability | Status |
|---|---|
| Read runtime state | Allowed |
| Observe artifacts | Allowed |
| Verify artifacts | Allowed |
| Mutate runtime state | **Forbidden** |
| Redefine protocol law | **Forbidden** |

### 4.3 Projection Operator

The projection operator is the semantic half of projection. It is normative. It returns a `ProjectionModel` — a view selection over the normal form. It does not return bytes or surface artifacts.

```
project : ViewSpec × NormalForm → Either RejectProject ProjectionModel
```

Surface emission is a separate, non-normative step performed by surface code after the operator returns:

```
emit : SurfaceClass × ProjectionModel → Either RejectProject SurfaceArtifact
```

where `SurfaceArtifact` is the concrete emitted representation (JSON, SVG, scene graph, NDJSON, etc.).

The full pipeline from normal form to rendered surface is therefore:

```
project(v, N) : Either RejectProject ProjectionModel   -- normative; operator law applies
emit(c, M)    : Either RejectProject SurfaceArtifact   -- non-normative; surface law applies
```

These two steps are never collapsed. `project` has no knowledge of `SurfaceClass`. `emit` has no access to `NormalForm`. This boundary prevents surface code from inventing semantics and prevents the operator from acquiring surface dependencies.

`ProjectionResult` in the type vocabulary therefore means `ProjectionModel` at the operator boundary. `SurfaceArtifact` is the product of `emit`, not of `project`.

### 4.4 Projection Laws

**Determinism**

```
project(v, N) = project(v, N)   -- always, for any v and N
```

No hidden state, clock time, randomness, or renderer heuristics may influence canonical output.

**Read-Only**  
If `project(v, N) = p`, then `p` is a representation of `N`, not a mutation of `N`.

**Semantic Preservation**

```
Sem(project(v, N)) = N   -- at the observation granularity admitted by v
```

**Boundary Preservation**

```
π(∂ℋ) ⊂ 𝒱   -- boundary-valid canonical states map to valid projection states
```

### 4.5 Projection Classes

| Class | Output form |
|---|---|
| P1 — document | Sections, text blocks, narrative beats |
| P2 — timeline | Ordered scrub sequence |
| P3 — graph | Nodes, edges, labels |
| P4 — spatial | Coordinates, transforms, geometry |
| P5 — immersive | Scene graph and immersive metadata |
| P6 — media | Synchronized audio/video attachment layout |
| P7 — composite | Synchronized multi-panel projection |

### 4.6 No-Backflow Rule

Projection surfaces must not introduce semantic authority.

| Forbidden Behavior | Reason |
|---|---|
| Runtime mutation | Breaks determinism |
| Protocol reinterpretation | Breaks authority boundaries |
| Artifact rewriting | Breaks traceability |

Surfaces may only: **observe**, **transform representation**, **render**.

### 4.7 Surface Homology

Different projection surfaces may represent the same runtime structure. These are homologous projections.

| Surface A | Surface B | Shared Invariant |
|---|---|---|
| Timeline | Narrative | Event ordering |
| Graph | Spatial | Topology |
| Dashboard | Timeline | State transitions |
| Document | Story | Beat sequence |

---

## 5. Decentralized Agent Model

### 5.1 Agent Obligations

| Obligation | Meaning |
|---|---|
| Schema conformance | Every write must be admitted by the shared schema |
| Canonical envelope | Writes must use SR-ABI SRCall format |
| Deterministic content | Same logical input produces the same canonical bytes |
| Stage declaration | Every write must declare its target stage |
| Reject transparency | Agents must propagate reject evidence faithfully |

### 5.2 Blackboard Invariants

The shared blackboard must maintain the following invariants at all times:

- All admitted content has passed `RejectRealize` — no unadmitted symbols exist on the board
- Closure is complete — the closed structure contains all relational consequences
- Normal form is canonical — the normalized structure has a unique representative for each equivalence class
- Schema preservation holds — no closure or normalization step has introduced unadmitted nodes or edges
- Projection surfaces observe only — no surface write has ever mutated canonical state

### 5.3 Confluence and Merge

The confluence law (§2.7) gives the blackboard its merge procedure:

> If two agents independently normalize the same document under the same schema, they must produce the same normal form.

This means agent writes are mergeably safe: concurrent writes to the blackboard that share a common prefix input cannot produce conflicting canonical states. Conflicts can only arise from genuinely different input documents, and those are resolved by the closure and normalization pipeline, not by arbitration.

### 5.4 Agent Pipeline Shape

```
Agent writes Document D
    ↓
realize(σ, D)    → RealizedStructure S         [or RejectRealize]
    ↓
close(σ, S)      → ClosedStructure S'          [or RejectClose]
    ↓
normalize(σ, S') → NormalForm N                [or RejectNormalize]
    ↓
canonical blackboard state updated with N
    ↓                                          ╌╌ ABI boundary ╌╌
project(v, N)    → ProjectionModel M           [or RejectProject]
    ↓                                          ╌╌ surface boundary ╌╌
emit(c, M)       → SurfaceArtifact             [or RejectProject]
    ↓
Observation surface renders artifact
```

`project` is normative and governed by the operator law. `emit` is non-normative and governed by the surface contract. They are separated by the surface boundary and must not be collapsed.

---

## 6. Execution ABI (SR-EABI)

### 6.1 Purpose

The SR-EABI (Stage Resolution Execution ABI) defines the executable realization of SR-ABI for host runtimes, VMs, device runtimes, and deterministic replay systems.

### 6.2 Commuting Square Law

Let:

```
resolveTo(s, x) : Either (R_s) (V_s)
encode_s        : Either (R_s) (V_s) → Bytes
decode_s        : Bytes → Either (R_s) (V_s)
exec_s          : Bytes_in → Bytes_out
```

The EABI requirement is:

```
decode_s(exec_s(encode_in(x))) = resolveTo(s, x)
```

> **Law:** The execution ABI is faithful to the canonical ABI when the commuting square holds for all stages `s` and all inputs `x`.

### 6.3 EABI Obligations

| Obligation | Meaning |
|---|---|
| Canonical faithfulness | Must realize SR-ABI exactly |
| No invented semantics | Must not introduce new computation |
| Deterministic execution | Same bytes in → same bytes out or same reject |
| Bounded memory | No unbounded allocation |
| Replay stability | Outputs are stable across replays |

---

## 7. Summary

### 7.1 The Blackboard Constitution

| Document | Role |
|---|---|
| Stage-Indexed Resolution | What the blackboard is allowed to mean |
| Pure Rewrite System | How blackboard content becomes lawful structure |
| SR-ABI | How agents exchange canonical board artifacts |
| Runtime Projection Interface | How observers read the board |
| Projection Operator | How canonical state becomes view state |
| Projection Surface Contract | What GUIs are forbidden from doing |

The system splits cleanly:

```
blackboard law
  = Stage-Indexed Resolution
  + Pure Rewrite System
  + SR-ABI

blackboard observation
  = Runtime Projection Interface
  + Projection Operator
  + Projection Surface Contract
```

### 7.2 The One-Line Shape

```
Document
  → realize      → RealizedStructure
  → close        → ClosedStructure
  → normalize    → NormalForm (canonical blackboard state)
  → project      → ProjectionModel          [ABI boundary]
  → emit         → SurfaceArtifact          [surface boundary]
```

### 7.3 The One-Line Law

> **The requested stage determines the codomain.**

### 7.4 Complete Law Inventory

| Law | Statement |
|---|---|
| Stage Soundness | `resolveTo(s, i) = Right v ⇒ v : ValueAt s` |
| Failure Monotonicity | Failure at stage `s` blocks all `t ≥ s` |
| Closure Idempotence | `close(σ, close(σ, S)) = close(σ, S)` |
| Normalization Idempotence | `normalize(σ, normalize(σ, S)) = normalize(σ, S)` |
| Confluence | `S ↠ N1` and `S ↠ N2` both normal ⇒ `N1 = N2` |
| Schema Preservation | Closure and normalization introduce no unadmitted nodes or edges |
| Projection Read-Only | `project(v, N)` returns a `ProjectionModel` that is a view of `N`, not a mutation |
| Operator/Emit Separation | `project` never returns `SurfaceArtifact`; `emit` never receives `NormalForm` |
| Commuting Square | `decode_s(exec_s(encode_in(x))) = resolveTo(s, x)` |
| No Backflow | Surfaces may not mutate canonical state or redefine protocol law |

### 7.5 Acceptance Criteria

The DBC is satisfied when:

- Every admitted document passes realization under the shared schema
- Closure is complete, idempotent, and schema-preserving
- Normalization is canonical, idempotent, confluent, and schema-preserving
- All projection surfaces consume only through the RPI
- No surface mutates canonical state
- All agents use SR-ABI canonical envelopes with correct `value_kind`
- `value_kind` for `Projected` results is always `ProjectionModel`; `SurfaceArtifact` never appears in an SRResult
- `project` and `emit` are not collapsed at any boundary
- The EABI commuting square holds for all stages

---

## Changelog

| Version | Change |
|---|---|
| DBC-1.0 | Initial specification |
| DBC-1.1 | Split `Structure` into `RealizedStructure` and `ClosedStructure`; added confluence law (rewrite-theoretic and distributed forms); clarified document order semantics; separated `ProjectionModel` from `SurfaceArtifact`; added schema preservation law at closure and normalization; added canonical vocabulary section; added complete law inventory |
| DBC-1.2 | Adopted Option A for projection operator: `project` returns `ProjectionModel` only; `emit` is a separate non-normative step; `SurfaceArtifact` is never a valid `value_kind` in SRResult; operator/emit separation added as a named law; staged resolver updated; agent pipeline shape updated with explicit ABI and surface boundaries; F-R3 fixture replaced with a total-order-safe normalization reject |

---

*DBC-1.2 · Deterministic Blackboard Calculus · Brian Thorne · bthornemail*
