# DBC / HD-RPC Specification Index

**DBC-INDEX · Stack Reference**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Living Document  

---

## The Stack

```
meaning (normal form)
  │
  ▼
DBC-1.2                    ← blackboard calculus (realize / close / normalize / project)
  │
  ▼
NormalForm
  │
  ▼
DBC-IDL-1.2                ← identity layer (SID · capabilities · adapters · descriptor)
  │
  ▼
DBC-NODE-1.0               ← node protocol (four HTTP endpoints)
  │
  ▼
HD-RPC-1.0                 ← umbrella architecture (semantic addressing · IPv6 · federation)
  │
  ▼
HD-RPC-GUI-1.0             ← role-based projection shell (observe / resolve / verify / project)
HD-RPC-EVR-1.0             ← canonical event registry (EventKind determines evidence shape)
  │
  ▼
new meaning                ← circular closure
```

---

## Documents

| Spec | Role | Audience |
|---|---|---|
| `HD-RPC-1.0-ARCHITECTURE` | Entry point — circular closure, three inversions, seven layers, audience maps | Everyone |
| `DBC-1.2` | Stage-indexed resolution calculus, rewrite system, SR-ABI envelopes | Language designers, runtime authors |
| `DBC-1.2-FIXTURES` | Canonical conformance test surface for DBC-1.2 | Implementers, CI systems |
| `DBC-IDL-1.2` | Identity layer: SID, capability authority, adapters, descriptor, projection | Identity system authors, federation architects |
| `DBC-NODE-1.0` | HTTP node protocol: four endpoints | Node operators, federation implementers |
| `HD-RPC-1.0` | Umbrella architecture: semantic addressing, IPv6 adapter, HD namespace | Architects, ecosystem integrators |
| `HD-RPC-1.0-DEPLOYMENT` | Deployment profiles A–D: single node, broker, federation, end-user | Operators, SaaS builders |
| `HD-RPC-1.0-SEMANTIC-ROUTING` | IPv6 adapter, zero-config nodes, circular closure, discovery without registries | Network architects, device integrators |
| `HD-RPC-GUI-1.0` | Role-based projection shell: five consoles, six workspaces, shell laws | GUI implementers, frontend architects |
| `HD-RPC-EVR-1.0` | Canonical event registry: seven families, 38 event kinds, evidence rules | Node authors, broker authors, GUI implementers |
| `DBC-TEST-MATRIX` | Test definitions for all six test classes | Conformance test authors, CI |
| `DBC-CONFORMANCE-MATRIX` | Law → test ID → pass/fail evidence mapping | Maintainers, auditors |

---

## Reading Order by Role

**Architect (full picture):** HD-RPC-1.0-ARCHITECTURE → HD-RPC-1.0 → HD-RPC-1.0-SEMANTIC-ROUTING

**Node operator:** DBC-1.2 → DBC-IDL-1.2 → DBC-NODE-1.0 → HD-RPC-1.0-DEPLOYMENT (Profile A)

**Federation operator:** DBC-NODE-1.0 §6 → HD-RPC-1.0 §7 → HD-RPC-1.0-DEPLOYMENT (Profile C)

**Broker / proxy:** DBC-NODE-1.0 §2–4 → HD-RPC-1.0-DEPLOYMENT (Profile B)

**Ecosystem adapter author:** DBC-IDL-1.2 §3 → DBC-NODE-1.0 §5 → HD-RPC-1.0 §5

**GUI implementer:** HD-RPC-GUI-1.0 → HD-RPC-EVR-1.0 (event subscriptions) → DBC-NODE-1.0 (endpoint contracts)

**Device / IoT integrator:** HD-RPC-1.0 §8 → HD-RPC-1.0-DEPLOYMENT (device section) → HD-RPC-EVR-1.0 Family G

**Conformance test author:** DBC-1.2 → DBC-1.2-FIXTURES → DBC-TEST-MATRIX → DBC-CONFORMANCE-MATRIX

**End-user client author:** DBC-IDL-1.2 §4 → DBC-NODE-1.0 §3 → HD-RPC-GUI-1.0 §6 (Projection Portal)

---

## The Doctrine

```
Identity is canonical.
Authority is delegated.
Credentials are adapters.
Descriptors are discoverable.
Presentation is projection.
Procedures are addressed by meaning.
Operators observe canonical truth; they do not author it.
EventKind determines evidence shape.
```

---

## The Complete Law Inventory

| Law | Spec |
|---|---|
| Stage Soundness | DBC-1.2 |
| Failure Monotonicity | DBC-1.2 |
| Closure Idempotence | DBC-1.2 |
| Normalization Idempotence | DBC-1.2 |
| Confluence | DBC-1.2 |
| Schema Preservation | DBC-1.2 |
| Projection Read-Only | DBC-1.2 |
| Operator/Emit Separation | DBC-1.2 |
| Commuting Square | DBC-1.2 |
| No-Backflow (calculus) | DBC-1.2 |
| SID Determinism | DBC-IDL-1.2 |
| Canonical Dependency | DBC-IDL-1.2 |
| Schema Pinning | DBC-IDL-1.2 |
| Meaning Primacy | DBC-IDL-1.2 |
| Canonical Encoding | DBC-IDL-1.2 |
| Domain Tag | DBC-IDL-1.2 |
| Namespace | DBC-IDL-1.2 |
| Capability Delegation | DBC-IDL-1.2 |
| No Self-Authorization | DBC-IDL-1.2 |
| Governor Chain | DBC-IDL-1.2 |
| Epoch-Boundedness | DBC-IDL-1.2 |
| Non-Confusion | DBC-IDL-1.2 |
| Adapter Isolation | DBC-IDL-1.2 |
| Non-Reversal | DBC-IDL-1.2 |
| Adapter Determinism | DBC-IDL-1.2 |
| Descriptor Non-Authority | DBC-IDL-1.2 |
| Descriptor Read-Only | DBC-IDL-1.2 |
| Descriptor Determinism | DBC-IDL-1.2 |
| Descriptor Epoch Validity | DBC-IDL-1.2 |
| DID Projection Read-Only | DBC-IDL-1.2 |
| Identity Projection Read-Only | DBC-IDL-1.2 |
| No-Backflow (identity) | DBC-IDL-1.2 |
| Calculus Faithfulness | DBC-NODE-1.0 |
| Artifact Immutability | DBC-NODE-1.0 |
| Descriptor Non-Authority (node) | DBC-NODE-1.0 |
| No Semantic Invention | DBC-NODE-1.0 |
| Schema Closure | DBC-NODE-1.0 |
| Capability Verification Completeness | DBC-NODE-1.0 |
| Adapter Isolation (node) | DBC-NODE-1.0 |
| Sensitive Credential Protection | DBC-NODE-1.0 |
| Semantic Address | HD-RPC-1.0 |
| HD Derivation | HD-RPC-1.0 |
| Call Determinism | HD-RPC-1.0 |
| Transport Neutrality | HD-RPC-1.0 |
| IPv6 Non-Authority | HD-RPC-1.0 |
| Namespace Without Registrar | HD-RPC-1.0 |
| Federation Convergence | HD-RPC-1.0 |
| Physical Uniformity | HD-RPC-1.0 |
| No Direct Mutation | HD-RPC-GUI-1.0 |
| Endpoint Fidelity | HD-RPC-GUI-1.0 |
| No-Backflow (shell) | HD-RPC-GUI-1.0 |
| Read-Only Projections | HD-RPC-GUI-1.0 |
| Role Scoping | HD-RPC-GUI-1.0 |
| Event-Driven Live Feeds | HD-RPC-GUI-1.0 |
| No Semantic Invention (shell) | HD-RPC-GUI-1.0 |
| Schema Closure Respect | HD-RPC-GUI-1.0 |
| Digest Verification | HD-RPC-GUI-1.0 |
| EventKind Determines Evidence Shape | HD-RPC-EVR-1.0 |
| Event Identity | HD-RPC-EVR-1.0 |
| Event Immutability | HD-RPC-EVR-1.0 |
| Emitter Completeness | HD-RPC-EVR-1.0 |
| Observer Non-Authority | HD-RPC-EVR-1.0 |
| No-Backflow (events) | HD-RPC-EVR-1.0 |
| Divergence Signal | HD-RPC-EVR-1.0 |

---

*DBC-INDEX · Brian Thorne · bthornemail*
