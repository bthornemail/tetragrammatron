# HD-RPC Architecture

**HD-RPC-1.0-ARCHITECTURE · Reference Document**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Normative Reference · Entry Point for the HD-RPC Stack  
Depends on: DBC-1.2 · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0  
Companion to: HD-RPC-1.0-DEPLOYMENT · HD-RPC-1.0-SEMANTIC-ROUTING · DBC-CONFORMANCE-MATRIX · DBC-TEST-MATRIX

---

## Purpose

This document is the single entry point for the HD-RPC architecture. It shows how all five specs compose into a self-stabilizing system, states the circular closure principle that makes the system semantically coherent, and gives each audience — architects, operators, implementers, conformance authors — the map they need to navigate the full spec set.

Everything described here is already specified. The architecture document adds nothing new; it makes the existing whole visible.

---

## 0. Canonical Vocabulary

| Term | Meaning |
|---|---|
| circular closure | the property that computation outputs are in the same semantic space as computation inputs — meaning produces meaning |
| HD-RPC | Hierarchical Deterministic Remote Procedure Call — a system in which procedure addresses are canonical semantic identities |
| semantic address | a SID used as a procedure endpoint, not a URL |
| layer | a stage of the architecture with a defined input, output, and law inventory |
| convergence | the property that all conforming nodes in a federation produce the same result for the same input |
| commuting square | the algebraic property that normalization and resolution commute — any path through the square yields the same canonical result |

---

## Document Structure

| Section | Role |
|---|---|
| §1 — The Three Inversions | What HD-RPC changes relative to conventional systems |
| §2 — Circular Closure | The property that distinguishes HD-RPC from open-loop architectures |
| §3 — The Seven Layers | The full architecture, layer by layer |
| §4 — The HD Namespace | How semantic structure produces a hierarchical procedure namespace |
| §5 — Federation and Convergence | Why nodes converge without consensus |
| §6 — The Conformance Model | How the law inventory maps to executable tests |
| §7 — Audience Maps | Which specs each role needs |
| §8 — The Complete Stack Diagram | The full architecture in one picture |

---

## 1. The Three Inversions

HD-RPC inverts three foundational assumptions of existing distributed systems. Each inversion follows from the one before it.

### Inversion 1 — Identity

```
conventional:  key       → identity
HD-RPC:        meaning   → identity
```

In conventional systems, identity is derived from a cryptographic key pair. Losing the key loses the identity. Key rotation changes the identity. The identity carries no semantic content about what the entity is.

In HD-RPC, identity is derived from a canonical normal form under a fixed schema. The key pair, if needed at all, is downstream — an adapter credential produced from the identity, not the source of it.

**Effect:** identity is stable across key rotation, node migration, and provider change. Two independently operating systems that normalize the same semantic structure under the same schema will derive the same identity without coordinating.

**Defined in:** DBC-IDL-1.2 §1

---

### Inversion 2 — Addressing

```
conventional:  registration  → address
HD-RPC:        identity      → address
```

In conventional systems, a network address is assigned by a registrar (DNS, DHCP, ENS, IPFS DHT). The address is external to the entity. The entity must register to become reachable.

In HD-RPC, a network address is derived from the identity using the `adapter:ipv6` adapter. The derivation is deterministic, domain-separated, and requires no registration. Any party that knows the SID can derive the address independently.

**Effect:** nodes are reachable from the moment they derive their own SID. Discovery is derivation.

**Defined in:** HD-RPC-1.0 §5, DBC-IDL-1.2 §3

---

### Inversion 3 — Procedures

```
conventional:  location   → procedure
HD-RPC:        meaning    → procedure
```

In conventional RPC, a procedure is named by a URL or string that encodes its server location. Moving the server changes the address. Renaming the procedure changes the address. Two providers of the same logical procedure have different addresses.

In HD-RPC, a procedure is named by its SID — derived from the canonical normal form of its semantic descriptor. The address is invariant under server migration, provider change, and implementation replacement. Two providers of the same logical procedure, under the same schema, have the same address.

**Effect:** procedures are portable. A consumer can switch providers without changing the procedure address.

**Defined in:** HD-RPC-1.0 §3, §4

---

## 2. Circular Closure

The deepest property of the HD-RPC architecture is that it is a **closed loop**.

Most distributed architectures are open loops. Their meaning is held externally — in documentation, human convention, or mutable registry state. The system computes, but it does not return to meaning.

```
open loop (REST, gRPC, Ethereum):
  input → computation → output
  (meaning is external)
```

HD-RPC is a closed loop. The output of computation is a canonical normal form — the same kind of object that serves as the input to identity derivation. The loop is:

```
meaning → identity → address → computation → meaning
```

This closure is not incidental. It follows from the DBC-1.2 Confluence and Normalization Idempotence laws:

```
normalize(resolve(S)) = resolve(normalize(S))
```

Computation and canonicalization commute. Any two paths through the system that reach the same stage produce the same canonical result. This is the algebraic property that makes federation convergence possible without a consensus protocol.

### The Commuting Square

```
         S  ──normalize──▶  NormalForm(S)
         │                       │
       resolve                 resolve
         │                       │
         ▼                       ▼
     Result(S)  ──normalize──▶  NormalForm(Result(S))
```

Any path through the square is equivalent. Nodes can execute in any order, cache intermediate results, replay historical calls, and still converge on the same canonical output.

**This is what makes HD-RPC a semantic internet rather than a semantic API.** An internet is a medium that is coherent regardless of path. HD-RPC achieves that coherence through the calculus, not through coordination.

### Comparison with Open-Loop Systems

| System | Structure | Closure |
|---|---|---|
| REST | endpoint → code → database | open — meaning is external |
| gRPC | package.service/method → protobuf codec | open — meaning is in IDL, not in runtime |
| Ethereum | key → address → contract → state | open — meaning is in ABI docs |
| IPFS | content → hash | partial — no computation semantics |
| DID | key or method → document | open — meaning is in method spec |
| HD-RPC | meaning → identity → address → computation → meaning | **closed** |

---

## 3. The Seven Layers

### Layer 0 — Meaning (DBC-1.2)

The system begins with documents submitted to the blackboard calculus.

```
document
  → realize(σ, D)   → RealizedStructure
  → close(σ, S)     → ClosedStructure
  → normalize(σ, S) → NormalForm
  → project(v, N)   → ProjectionModel
```

The calculus is governed by nine laws (Stage Soundness, Failure Monotonicity, Closure Idempotence, Normalization Idempotence, Confluence, Schema Preservation, Projection Read-Only, Operator/Emit Separation, Commuting Square). These laws guarantee that the same document under the same schema always produces the same normal form, regardless of which node runs the calculus or in what order rewrite rules are applied.

**What this layer produces:** canonical normal forms — the input to every other layer.

**Key invariant:** `same canonical input → same canonical output`

---

### Layer 1 — Identity (DBC-IDL-1.2)

From a canonical normal form, a semantic identity is derived:

```
SID = H(
  tag("schema_digest")     || encode(schema_digest),
  tag("normal_form")       || encode(canonical_normal_form),
  tag("federation_scope")  || encode(federation_scope),
  tag("derivation_context")|| encode(derivation_context)
)
```

prefixed with domain tag `"DBC-SID-1.1\0"`.

From the SID, the identity layer produces four downstream artifacts:

```
SID
  ├── CapabilityGrant     [who may act for this SID]
  ├── AdapterCredential   [how this SID appears in ecosystem X]
  ├── IdentityDescriptor  [public resolution surface — replaces DID registries]
  └── IdentityProjection  [observational presentation — non-authoritative]
```

**What this layer produces:** a stable, content-derived identifier that is portable across key rotation, node migration, and provider change.

**Key invariant:** `same meaning → same identity`

---

### Layer 2 — Descriptor (DBC-IDL-1.2 §4)

The `IdentityDescriptor` is the artifact that allows external systems to discover how to interact with a SID. It contains the governors, active adapter bindings, verification methods, revocation references, and service endpoints for a given SID.

```json
{
  "sid":              "sid:dbc:<digest>",
  "spec":             "DBC-IDL-1.2",
  "schema_digest":    "sha256:...",
  "federation_scope": "...",
  "epoch":            "...",
  "governors":        [...],
  "adapters":         {...},
  "revocation":       {...},
  "services":         [...],
  "descriptor_digest":"sha256:..."
}
```

The descriptor is produced by projection from governed state. It is never the source of truth. If the descriptor and governed state disagree, governed state is correct.

**What this layer produces:** a canonical, read-only, content-addressable service document — without a registry.

**Key invariant:** `IdentityDescriptor ↛ SID`

---

### Layer 3 — Node (DBC-NODE-1.0)

A node is a governed artifact host that binds layers 0–2 to four HTTP endpoints:

```
POST /resolve            ← runs resolveTo(stage, input)
GET  /sid/{digest}       ← serves IdentityDescriptor
POST /verify-capability  ← verifies capability grant chain
GET  /adapter/{label}/{sid} ← derives ecosystem credential
```

The node runs the calculus faithfully, stores canonical artifacts immutably, projects descriptors from governed state, and never invents semantic content. It is a compute surface for the calculus, not an authority over identity.

**What this layer produces:** a runtime that agents can call, federations can trust, and ecosystem adapters can query.

**Key invariant:** `same SRCall → same SRResult` (on any conforming node in the same federation)

---

### Layer 4 — HD-RPC (HD-RPC-1.0)

HD-RPC names the complete architecture and adds three properties above the node protocol:

**Semantic addressing:** procedure endpoints are SIDs, not URLs.

```
call(SID(service/method), Normalized)
```

instead of

```
POST https://api.example.com/service/method
```

**HD namespace:** nested semantic structures produce nested SIDs deterministically, analogous to BIP32 HD wallet derivation but over meaning rather than keys.

```
SID(org)
SID(org/repo)
SID(org/repo/module)
SID(org/repo/module/fn)
```

Any party can derive the correct SID from the schema and descriptor without a registry.

**Transport neutrality:** the SID is invariant across transports. The same procedure can be called over HTTPS, CoAP, libp2p, or raw IPv6 socket without changing its address.

**What this layer adds:** the naming convention that makes the node protocol into a callable procedure namespace.

---

### Layer 5 — Semantic Routing (HD-RPC-1.0 §5, §6)

The `adapter:ipv6` adapter derives a 128-bit IPv6 address from a SID:

```
ipv6 = HKDF(
  ikm    = sid_bytes,
  info   = "adapter:ipv6",
  salt   = federation_scope_bytes,
  length = 16
)
address = fd00::/8 prefix + ipv6[0:15]
```

This makes every semantic identity physically routable without DNS, DHCP, or any registry. A node that knows a service SID can derive its IPv6 address and connect directly. A node configures its own address from its own SID — zero-configuration, deterministic, globally unique within its IPv6 ULA prefix.

The full routing path collapses to:

```
SID(service)  →  adapter:ipv6  →  connect  →  POST /resolve
```

**What this layer adds:** the physical routing substrate that makes the semantic network reachable on existing IPv6 infrastructure.

**Key invariant:** `IPv6 address is routing handle only — it does not define the SID or authorize any capability`

---

### Layer 6 — Ecosystem Adapters (DBC-IDL-1.2 §3)

Every SID can be projected into any supported ecosystem via domain-separated HKDF derivation:

| Adapter | Output | Use |
|---|---|---|
| `adapter:evm` | 20-byte Ethereum address | DeFi, smart contracts |
| `adapter:bip39` | HD wallet mnemonic | key management |
| `adapter:did:key` | W3C DID document | verifiable credentials |
| `adapter:git` | Git object reference | code identity |
| `adapter:ipfs` | CIDv1 content ID | content addressing |
| `adapter:ipv6` | 128-bit routing address | network routing |
| `adapter:device:<class>` | device key | hardware identity |
| `adapter:oauth:<provider>` | provider principal | legacy SSO |

All adapters satisfy three laws: Adapter Determinism (same SID + domain → same credential), Adapter Isolation (adapters are computationally independent across domains), and Non-Reversal (credentials cannot recover the SID).

One meaning. Many ecosystem forms. All derived, none authoritative over the SID.

---

## 4. The HD Namespace

The "hierarchical deterministic" property of HD-RPC names a specific structural feature: semantic containment in the source domain produces deterministic SID derivation in the address domain.

In BIP32, a key derivation tree gives every wallet, account, and key a unique address derivable from a master seed. The tree structure is imposed by the derivation path.

In HD-RPC, the tree structure is not imposed — it emerges from the semantic structure of the domain being normalized. An organization contains repositories; a repository contains modules; a module contains functions. Normalization at each level produces a NormalForm that includes the containing structure. The SID therefore encodes the containment hierarchy.

```
normalize(fn | module | repo | org) → NormalForm_fn
SID(NormalForm_fn) = H(schema, NormalForm_fn, scope, context)
```

This is strictly more powerful than BIP32 derivation because:

- the tree structure is not arbitrary — it is the canonical structure of the semantic domain
- any party can derive the correct SID from the descriptor and schema without access to any secret
- the namespace requires no registrar — two independent parties normalizing the same function descriptor under the same schema will arrive at the same SID

The result is a **global, self-organizing procedure namespace** in which every callable entity has a unique, content-derived, publicly derivable address.

---

## 5. Federation and Convergence

A federation is a set of HD-RPC nodes that share a schema set, federation scope, trust anchors, and epoch definition.

The convergence property follows from two laws:

**DBC-1.2 Confluence:** any two rewrite sequences that normalize the same document under the same schema produce the same NormalForm.

**DBC-IDL-1.2 SID Determinism:** same NormalForm + same schema + same scope + same context → same SID.

Therefore, for any two honest nodes N1 and N2 in the same federation, with the same document D and schema σ:

```
normalize(σ, D) = N          (same NormalForm, by Confluence)
SID(N, σ, scope, ctx) = s    (same SID, by SID Determinism)
```

The nodes agree on the identity of every artifact without communicating. **Identity is ambient in a shared calculus.**

This is why HD-RPC federation does not require a consensus protocol for identity. Consensus is only needed for two things:

- which schemas are admitted (schema governance)
- what epoch is current (time coordination)

Everything else — normal forms, SIDs, descriptors, adapter credentials — follows deterministically from those two shared inputs.

### Geometric Interpretation

A DBC federation of honest nodes is a distributed realization of a single canonical apartment in the Tits building sense. The shared calculus defines the building type. Each node hosts a chamber. Normalization is retraction onto the apartment. Confluence is the intersection property: any two chambers share a canonical common apartment.

This is the internal correctness geometry. It is not the operational model. But it is the reason the system remains coherent as it scales: the architecture is not just engineered to converge — it is geometrically constrained to converge.

---

## 6. The Conformance Model

The law inventory across the full stack contains 47 named laws, organized into six families:

| Family | Laws | Spec |
|---|---|---|
| Calculus | Stage Soundness · Failure Monotonicity · Closure/Normalization Idempotence · Confluence · Schema Preservation · Projection Read-Only · Operator/Emit Separation · Commuting Square · No-Backflow | DBC-1.2 |
| Identity | SID Determinism · Canonical Dependency · Schema Pinning · Meaning Primacy · Canonical Encoding · Domain Tag · Namespace | DBC-IDL-1.2 |
| Capability | Capability Delegation · No Self-Authorization · Governor Chain · Epoch-Boundedness · Non-Confusion | DBC-IDL-1.2 |
| Adapter | Adapter Isolation · Non-Reversal · Adapter Determinism · Descriptor laws (×5) · DID/Identity Projection Read-Only · No-Backflow | DBC-IDL-1.2 |
| Node | Calculus Faithfulness · Artifact Immutability · Descriptor Non-Authority · No Semantic Invention · Schema Closure · Capability Verification Completeness · Adapter Isolation · Sensitive Credential Protection | DBC-NODE-1.0 |
| HD-RPC | Semantic Address · HD Derivation · Call Determinism · Transport Neutrality · IPv6 Non-Authority · Namespace Without Registrar · Federation Convergence · Physical Uniformity | HD-RPC-1.0 |

These laws map to executable tests in the DBC-TEST-MATRIX across six test classes:

| Test class | What it proves |
|---|---|
| Golden | deterministic correctness — same canonical input → same canonical output |
| Negative | reject surface — failures are typed, deterministic, and protocol-correct |
| Extension | schema evolution — new rules do not break existing determinism |
| Adapter | ecosystem projection — deterministic, isolated, non-reversible |
| Integration | full pipeline — SRCall flows correctly through every layer |
| Replay | cross-runtime convergence — federation nodes agree under independent execution |

The DBC-CONFORMANCE-MATRIX maps every law to specific test IDs and specifies what constitutes pass/fail evidence. The minimal initial conformance set is:

```
G-01, G-02, G-03, G-04,   ← core calculus correctness
N-01, N-02, N-03, N-04,   ← primary reject surface
G-09,                      ← commuting square
A-01, A-03, A-05, A-08,   ← adapter determinism and domain separation
I-01, I-06, I-07,         ← node faithfulness, broker non-invention, federation convergence
R-01, R-05                ← cross-runtime and federation replay
```

A stack implementation is conformant only when all required tests pass with exact canonical evidence — not approximate equivalence.

---

## 7. Audience Maps

### Architects

Read this document in full. Then: HD-RPC-1.0 (semantic addressing, IPv6 adapter, HD namespace), HD-RPC-1.0-SEMANTIC-ROUTING (circular closure, commuting square, discovery without registries).

### Node Operators

DBC-1.2 (to understand what the calculus requires) → DBC-IDL-1.2 (to understand the identity and descriptor model) → DBC-NODE-1.0 (to implement the four endpoints) → HD-RPC-1.0-DEPLOYMENT Profile A.

### Federation Operators

DBC-NODE-1.0 §6 (federation protocol) → HD-RPC-1.0 §7 (federation convergence property) → HD-RPC-1.0-DEPLOYMENT Profile C.

### Broker / Proxy Implementers

DBC-NODE-1.0 §2–§4 (resolve, descriptor, capability) → HD-RPC-1.0-DEPLOYMENT Profile B. Key law: No Semantic Invention — the broker routes; it does not produce.

### Ecosystem Adapter Authors

DBC-IDL-1.2 §3 (adapter derivation laws) → DBC-NODE-1.0 §5 (adapter endpoint contract) → HD-RPC-1.0 §5 (IPv6 adapter as reference implementation).

### Conformance Test Authors

DBC-1.2 → DBC-1.2-FIXTURES → DBC-TEST-MATRIX → DBC-CONFORMANCE-MATRIX.

### Device / IoT Integrators

HD-RPC-1.0 §8 (device participation) → HD-RPC-1.0-DEPLOYMENT device section → HD-RPC-1.0-SEMANTIC-ROUTING device section. Minimum requirements: SHA-256 or BLAKE3, HKDF, four node endpoints over at least one transport.

### End-User Client Authors

DBC-IDL-1.2 §4 (descriptor form) → DBC-NODE-1.0 §3 (GET /sid/{digest}) → HD-RPC-1.0-DEPLOYMENT Profile D. Key law: No-Backflow — projection surfaces are read-only.

---

## 8. The Complete Stack

### The Architecture

```
                   ┌──────────────────────────────────┐
                   │          meaning                 │
                   │      (semantic structure)        │
                   └────────────────┬─────────────────┘
                                    │ DBC-1.2
                                    ▼
                   ┌──────────────────────────────────┐
                   │          NormalForm              │
                   │   (canonical semantic artifact)  │
                   └────────────────┬─────────────────┘
                                    │ DBC-IDL-1.2
                                    ▼
                   ┌──────────────────────────────────┐
                   │             SID                  │
                   │  ├── CapabilityGrant             │
                   │  ├── AdapterCredential           │
                   │  │   ├── adapter:evm             │
                   │  │   ├── adapter:did:key         │
                   │  │   ├── adapter:ipv6  ◀── routing
                   │  │   └── …                      │
                   │  ├── IdentityDescriptor          │
                   │  └── IdentityProjection          │
                   └────────────────┬─────────────────┘
                                    │ DBC-NODE-1.0
                                    ▼
                   ┌──────────────────────────────────┐
                   │             Node                 │
                   │  POST /resolve                   │
                   │  GET  /sid/{digest}              │
                   │  POST /verify-capability         │
                   │  GET  /adapter/{label}/{sid}     │
                   └────────────────┬─────────────────┘
                                    │ HD-RPC-1.0
                                    ▼
                   ┌──────────────────────────────────┐
                   │          HD-RPC call             │
                   │      call(SID, stage)            │
                   │        SRCall → SRResult         │
                   └────────────────┬─────────────────┘
                                    │ IPv6 / HTTPS / LoRa / BLE
                                    ▼
                   ┌──────────────────────────────────┐
                   │         new meaning              │  ← circular closure
                   │     (new NormalForm)             │
                   └──────────────────────────────────┘
```

### The Doctrine

```
Identity is canonical.
Authority is delegated.
Credentials are adapters.
Descriptors are discoverable.
Presentation is projection.
Procedures are addressed by meaning.
```

### The One-Line Definition

> A semantic internet where meaning deterministically produces identity, identity produces addresses, addresses produce computation, and computation returns to meaning.

---

## Changelog

| Version | Change |
|---|---|
| HD-RPC-1.0-ARCHITECTURE | Initial reference document. Defines the circular closure principle, the three architectural inversions, the seven-layer model, the HD namespace property, the federation convergence argument, the conformance model summary, and audience maps across all seven implementer roles. |

---

*HD-RPC-1.0-ARCHITECTURE · Brian Thorne · bthornemail*
