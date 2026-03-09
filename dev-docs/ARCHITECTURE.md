# Tetragrammatron · Architecture

**ARCHITECTURE-1.0**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Canonical reference  
See also: [README.md](./README.md) · [ROADMAP.md](./ROADMAP.md)

---

## One-Line Definition

> **Tetragrammatron is a deterministic semantic infrastructure stack where canonical meaning produces identity, identity produces addressability, and computation returns to meaning.**

---

## The Fundamental Loop

```
meaning
  │
  ▼  normalize
canonical form
  │
  ▼  deriveSID
identity (SID)
  │
  ▼  adapter:ipv6 / adapter:*
address
  │
  ▼  POST /resolve
computation
  │
  ▼  project
meaning
```

This loop closes on itself. The same canonical meaning that produces the SID on one node produces the same SID on every other node, independently, without coordination. This is the foundation of the federation model.

---

## The Three Inversions

The architecture inverts three assumptions that are conventional in distributed systems.

| Conventional | Tetragrammatron |
|---|---|
| Generate a key → derive identity from it | Normalize meaning → derive identity from canonical form |
| Register a service → assign it an address | Derive identity from meaning → derive address from identity |
| Locate a service → call it at its address | Address meaning → computation follows identity |

These inversions are why the system has no registries, no central authorities, and no bootstrapping problem for identity or addressing.

---

## The Five Layers

### Layer 1 — Substrate

**Specification:** NRR-1.0  
**Repository:** tetragrammatron-substrate  
**Principle:** *The substrate stores; it does not mean.*

The substrate is a hash-indexed append-only log with deterministic replay. It provides content-addressed blob storage, an ordered log of typed entries, and replay from any checkpoint.

The substrate has no concept of schema, identity, routing, or semantics. It knows only: bytes go in under a hash, bytes come out verified by the same hash, entries are appended and never modified, and replay from the same log always produces the same result.

Everything above the substrate can be rebuilt from it. Nothing above it is required for substrate correctness.

**Six substrate laws:**
- Content Address Law: `ref = hash(bytes)`
- Append-Only Law: log entries never modified or deleted
- Replay Law: same log and blob set → same state, on any conforming runtime
- Artifact Immutability Law: once stored under ref R, content never changes
- Transport Neutrality Law: correctness requires no transport
- Authority Neutrality Law: substrate defines no identity or authorization

---

### Layer 2 — Protocol

**Specifications:** DBC-1.2 · DBC-IDL-1.2  
**Repository:** tetragrammatron-protocol  
**Principle:** *Meaning determines identity.*

The protocol layer defines two things: how semantic structures are normalized to canonical form, and how canonical form deterministically produces identity.

**DBC-1.2** defines the four-stage resolution calculus:

```
Realized < Closed < Normalized < Projected
```

A document enters as a `Realized` structure (admitted symbols, valid surface syntax), is closed under semantic rules, normalized to a canonical representative form, and optionally projected to a surface artifact for a specific audience.

The critical property is the Commuting Square:

```
normalize(resolve(S)) = resolve(normalize(S))
```

Any path through the resolution stages produces the same canonical result. This is why federation converges without consensus.

**DBC-IDL-1.2** defines identity as structural:

```
SID = H("DBC-SID-1.1\0" || TLV(schema_digest, normal_form, federation_scope, derivation_context))
```

The SID is derived from the NormalForm. The same NormalForm always produces the same SID. Identity is not assigned; it is derived. This eliminates the registration problem.

**IDL-1.2 also defines:**
- `CapabilityGrant` — bounded authority delegation
- `AdapterCredential` — controlled projection into external ecosystems
- `IdentityDescriptor` — registry-free self-describing identity document

**Key doctrinal boundary:** the protocol layer defines laws about meaning and identity. It does not define execution, transport, or presentation.

---

### Layer 3 — Core

**Specification:** DBC-NODE-1.0  
**Repository:** tetragrammatron-core  
**Principle:** *The node executes meaning without inventing semantics.*

The core layer binds the protocol to execution. A node is an HTTP server that runs the DBC calculus, stores canonical artifacts in NRR, derives and serves identity descriptors, and verifies capability chains.

**Four canonical endpoints:**

| Endpoint | Operation |
|---|---|
| `POST /resolve` | Run the calculus. Accept an `SRCall`, return an `SRResult`. |
| `GET /sid/{digest}` | Serve the `IdentityDescriptor` for a SID. |
| `POST /verify-capability` | Verify a capability grant chain. Returns one of six typed results. |
| `GET /adapter/{label}/{sid}` | Derive an ecosystem credential from a SID. |

**Key doctrinal boundary:** the node faithfully executes the calculus and serves what canonical state dictates. It introduces no semantic content. `No Semantic Invention` is a hard law: if the calculus does not produce it, the node does not serve it.

---

### Layer 4 — Network

**Specification:** HD-RPC-1.0 (including semantic routing and deployment profiles)  
**Repository:** tetragrammatron-network  
**Principle:** *Identity determines address.*

The network layer provides semantic addressing, routing, and federation. Its scope is intentionally narrow: naming and routing only. It is not a generic distributed systems layer.

**Semantic addressing:**

```
call(SID, stage)
```

rather than:

```
POST https://api.example.com/method
```

The SID is the address. The node that holds the canonical artifacts for that SID is reachable because identity and addressing are co-derived.

**HD Namespace:** nested semantic structures produce nested SIDs:

```
SID(org) → SID(org/repo) → SID(org/repo/module) → SID(org/repo/module/fn)
```

**adapter:ipv6:** the IPv6 semantic routing adapter derives a stable ULA address from the SID:

```
seed = HKDF(sid_bytes, "adapter:ipv6", federation_scope_bytes, length=16)
ipv6 = fd00::/8 prefix + seed[0:15]
```

A node derives its own SID from its descriptor, derives its IPv6 address from its SID, and configures its network interface — zero-config, no DHCP, no DNS, no registry.

**Federation:** nodes sharing a schema set and federation scope converge mathematically. The Commuting Square guarantees that two nodes normalizing the same document always produce the same NormalForm and the same SID, and therefore the same derived address. Convergence is a mathematical property, not a consensus protocol.

**Key doctrinal boundary:** network scope is semantic addressing, routing, and federation discovery. Generic infrastructure, adapters, and presentation surfaces belong in other layers.

---

### Layer 5 — Hub

**Specifications:** HD-RPC-GUI-1.0 · HD-RPC-EVR-1.0  
**Repository:** tetragrammatron-hub  
**Principle:** *Projections observe truth; they do not define it.*

The hub provides two surfaces: the role-based projection shell (GUI) and the canonical event registry (EVR).

**HD-RPC-GUI-1.0** is a shell specification, not an application specification. It defines:
- five role-scoped consoles (Provider · Broker · Consumer · User · Agent)
- six top-level workspaces backed by the four node endpoints
- nine shell laws, the most important of which is No Direct Mutation

All writes enter the system through `POST /resolve`. The shell presents canonical artifacts; it never constructs them.

**HD-RPC-EVR-1.0** is the canonical event taxonomy. It defines:
- seven event families (Resolution · Identity · Capability · Adapter · Routing · Federation · Cyber-physical)
- 38 named event kinds, each with required evidence fields
- the law: *EventKind determines evidence shape*

EVR is conceptually cross-cutting: it provides the observability contract for nodes, brokers, the GUI, and device agents. It lives in the Hub repository for operational reasons but its laws apply across the entire stack.

---

## The Adapters

**Repository:** tetragrammatron-adapters

Adapters are controlled projections into external ecosystems. They are not semantic sources; they are semantic derivatives.

| Adapter | Derives from SID | Produces |
|---|---|---|
| adapter:ipv6 | HKDF(sid, "adapter:ipv6", scope) | ULA IPv6 address |
| adapter:evm | HKDF(sid, "adapter:evm", scope) | Ethereum address |
| adapter:did:key | HKDF(sid, "adapter:did:key", scope) | DID document |
| adapter:git | HKDF(sid, "adapter:git", scope) | Git remote handle |
| adapter:ipfs | HKDF(sid, "adapter:ipfs", scope) | IPFS CID |
| adapter:bip39 | HKDF(sid, "adapter:bip39", scope) | HD wallet root — sensitive, requires capability |

Three adapter laws:
- Deterministic derivation: same SID + same label → same credential
- Isolation: adapter credentials are not admitted into the calculus
- Non-authority: adapter credentials do not alter the SID or the descriptor

---

## Fixtures and Conformance

**Fixtures repository:** tetragrammatron-fixtures  
Golden artifacts, reference descriptors, canonical event streams, and replay traces used across all five layers.

**Conformance repository:** tetragrammatron-conformance  
Every doctrinal law maps to one or more executable tests. The conformance matrix is the auditable record that an implementation satisfies the stack.

The complete law inventory spans 70 named laws across 9 specifications.

---

## The Complete Doctrine

```
Identity is canonical.
Authority is delegated.
Credentials are adapters.
Descriptors are discoverable.
Presentation is projection.
Procedures are addressed by meaning.
Operators observe canonical truth; they do not author it.
EventKind determines evidence shape.
The substrate stores; it does not mean.
```

---

## Specification Index

| Specification | Layer | Role |
|---|---|---|
| NRR-1.0 | Substrate | content-addressed log and replay |
| DBC-1.2 | Protocol | stage-indexed resolution calculus |
| DBC-IDL-1.2 | Protocol | identity: SID · capability · adapter · descriptor |
| DBC-NODE-1.0 | Core | node protocol and four endpoints |
| HD-RPC-1.0 | Network | semantic addressing · IPv6 · federation |
| HD-RPC-1.0-SEMANTIC-ROUTING | Network | IPv6 adapter and zero-config nodes |
| HD-RPC-1.0-DEPLOYMENT | Network | deployment profiles A–D |
| HD-RPC-GUI-1.0 | Hub | role-based projection shell |
| HD-RPC-EVR-1.0 | Hub | canonical event registry |

Supporting documents: DBC-1.2-FIXTURES · DBC-TEST-MATRIX · DBC-CONFORMANCE-MATRIX · DBC-INDEX

---

*Tetragrammatron · ARCHITECTURE-1.0 · Brian Thorne · bthornemail*
