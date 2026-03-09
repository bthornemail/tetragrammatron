# HD-RPC-1.0 · Service Architecture and Deployment Profiles

**Author:** Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
**Status:** Draft · Companion to HD-RPC-1.0  
**Depends on:** DBC-1.2 · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0

---

## Doctrine

> **Providers publish meaning. Brokers route identity. Consumers request stages. Users receive projections.**

```
Identity is canonical.
Authority is delegated.
Credentials are adapters.
Descriptors are discoverable.
Presentation is projection.
Procedures are addressed by meaning.
```

---

## The Adoption Model

HD-RPC is a **deterministic service mesh with canonical identity, typed resolution, and registry-free discovery.**

Operators, providers, brokers, and consumers each interact with a different surface of the same underlying system. The algebraic model is always present internally — but the operational surface is what each role deploys, publishes, routes, or receives.

The service-facing vocabulary:

| Protocol term | Service-facing meaning |
|---|---|
| schema | service contract |
| normal form | canonical result |
| SID | semantic service identity |
| descriptor | registry-free service document |
| capability | authorization token |
| adapter | ecosystem bridge |
| projection | UI / API presentation surface |
| SRCall | canonical request envelope |
| SRResult | canonical response envelope |

---

## Role Map

```
Provider
  publishes schema + resolver + descriptor + adapters
        │
        ▼
Broker / Proxy
  caches descriptors · verifies capabilities · routes SR-ABI calls
        │
        ▼
Consumer
  sends SRCall · receives SRResult
        │
        ▼
End-user
  sees projected identity + projected results
```

This maps directly to the published stack:

```
DBC-1.2          ← computation (realize · close · normalize · project)
DBC-IDL-1.2      ← identity (SID · capabilities · adapters · descriptor)
DBC-NODE-1.0     ← node protocol (four HTTP endpoints)
HD-RPC-1.0       ← umbrella (semantic addressing · IPv6 routing · federation)
Ecosystem        ← evm · bip39 · did:key · git · ipfs · ipv6 · device · oauth
```

---

## What Each Role Does

### Provider

A provider publishes:

- a **schema** — the service contract under which all normal forms are derived
- a **node** — four HTTP endpoints: `POST /resolve`, `GET /sid/{digest}`, `POST /verify-capability`, `GET /adapter/{label}/{sid}`
- a **descriptor** — the registry-free service document, retrievable at `GET /sid/{digest}`
- **adapters** — optional ecosystem bridges (EVM address, DID, Git commit key, IPv6 routing handle)

A provider does **not** publish arbitrary endpoints as the primary abstraction. The node protocol is the operational binding. The semantic service identity is the stable address.

### Broker / Proxy

A broker:

- caches and routes **descriptors**
- verifies **capability chains** before forwarding calls
- proxies **SR-ABI calls** to provider nodes
- **never redefines semantics** — it is a descriptor-aware SR-ABI gateway

A broker can be implemented as a stateless gateway that holds no canonical state. All canonical state lives at provider nodes.

### Consumer

A consumer:

- resolves by **semantic identity** — sends `SID` and desired `stage`
- sends a **SRCall** envelope
- receives a **SRResult** envelope

No URL construction, no registry lookup, no ad hoc versioning. The SID is the stable address for as long as the meaning is stable.

### End-user

An end-user never sees the resolution calculus directly. They see:

- **Verified Service** — descriptor-confirmed identity
- **Canonical Identity** — the SID-backed subject
- **Authorized Actor** — a verified capability chain
- **Current Descriptor** — the live resolution surface
- **Deterministic Result** — the projected output

---

## Deployment Profiles

### Profile A — Single-Service Canonical Resolver

**For:** one provider, one schema set, one node.  
**Use when:** a provider wants canonical service identity, deterministic request/response, and descriptor-based discovery.

**Minimal deployment:**

```
1 node
1 schema set
artifact store
descriptor store
capability verifier
```

**Sequence:**

```
Consumer → POST /resolve (SRCall)
               │
               ▼
           Node: realize → close → normalize → project
               │
               ▼
Consumer ← SRResult
```

**What the provider deploys:**

```
node
  schema set (pinned digest)
  artifact store (immutable, append-only)
  descriptor store (GET /sid/{digest})
  capability verifier (POST /verify-capability)
```

**Adapters to enable first:** `adapter:did:key` for DID projection · `adapter:ipv6` for semantic routing

---

### Profile B — Broker / Proxy Federation

**For:** SaaS brokers, reverse proxies, API gateways.  
**Use when:** a broker needs to route SR-ABI calls across multiple providers without requiring each provider to be directly reachable.

**Topology:**

```
Consumer
    │  SRCall
    ▼
Broker
    ├── descriptor cache
    ├── capability verifier
    └── SR-ABI router
         │
         ├──▶ Provider Node A
         ├──▶ Provider Node B
         └──▶ Provider Node C
```

**Broker responsibilities:**

- Resolve SID to descriptor via `GET /sid/{digest}` on provider nodes
- Cache descriptors with epoch validity (never serve expired descriptors)
- Verify capability grants before forwarding any call
- Forward `SRCall` verbatim to the canonical provider node
- Return `SRResult` verbatim to the consumer

**What the broker must never do:**

- Rewrite the SRCall payload
- Issue capability grants on behalf of providers
- Serve its own SID as authoritative for a provider's semantic content

**Law enforced:** No Semantic Invention — the broker routes; it does not produce.

---

### Profile C — Multi-Provider Semantic Federation

**For:** providers sharing a federation scope.  
**Use when:** multiple providers share a schema set, trust anchors, and epoch coordination.

**Shared federation state:**

```
federation_scope   (stable identifier for this federation)
schema set         (pinned digest, shared by all members)
trust anchors      (root capability governors)
epoch schedule     (coordinated rotation for time-bounded capabilities)
FIDX               (federation index — discoverable by SID)
```

**Node topology:**

```
Federation Scope
    │
    ├── Node A (provider: service-X)
    │     schema: sha256:abc…
    │     federation_scope: did:fed:prod
    │
    ├── Node B (provider: service-Y)
    │     schema: sha256:abc…
    │     federation_scope: did:fed:prod
    │
    └── Node C (broker / gateway)
          caches: descriptors for A and B
          verifies: capability chains rooted at trust anchors
```

**Convergence property:** any honest node in the federation derives the same SID for the same document under the same schema. No gossip protocol or consensus mechanism is required for identity — convergence is a mathematical consequence of SID Determinism.

**What federation adds over Profile B:**

- shared schema digest ensures cross-node normal form equivalence
- shared trust anchors make capability chains portable across nodes
- FIDX allows registry-free federation discovery

---

### Profile D — End-User Projection Network

**For:** browsers, wallets, devices, portals — any consumer that should never touch the resolution calculus directly.

**What end-user clients receive:**

```
IdentityDescriptor   → who/what this SID represents
AdapterCredential    → ecosystem-specific form (EVM address, DID, etc.)
IdentityProjection   → presentation surface (rendered identity card)
SRResult (Projected) → canonical output at stage = Projected
```

**What end-user clients never receive:**

- raw normal forms
- capability grant internals
- schema digests (unless explicitly requested for verification)

**No-backflow law enforced:** projection surfaces are read-only. A consumer receiving a projection cannot push state back into the calculus through the projection interface.

**Minimal client interaction:**

```
GET /sid/{digest}              → IdentityDescriptor
GET /adapter/did:key/{sid}     → DID credential
POST /resolve (stage: Projected) → rendered projection result
```

---

## IPv6 Semantic Routing

When `adapter:ipv6` is enabled, every SID maps deterministically to a 128-bit IPv6 address. Nodes configure this address directly on their network interface — no DNS, no DHCP, no registry.

**Derivation:**

```
ipv6_bytes = HKDF(
  ikm    = sid_bytes,
  salt   = federation_scope_bytes,
  info   = "adapter:ipv6",
  length = 16
)
ipv6_address = fd00::/8 prefix + ipv6_bytes[0:15]
```

**Routing consequence:** any node that knows a provider's SID can derive its IPv6 address and reach it directly — subject only to network connectivity. This is semantic routing: address derivation from meaning, not from registration.

**IPv6 Non-Authority law:** the IPv6 address is a routing handle only. It does not define the SID, does not authorize any capability, and is not a substitute for SID verification.

---

## Device Participation

Constrained devices (ESP32, Raspberry Pi, LoRa gateway) participate as full HD-RPC nodes if they can:

1. compute SHA-256 or BLAKE3
2. run HKDF
3. serve the four node endpoints over at least one transport

A device derives its own SID and IPv6 address from its normalized hardware descriptor:

```
device_sid  = SID(normalize(device_descriptor), schema, federation_scope)
device_ipv6 = adapter(device_sid, "adapter:ipv6")
```

The device configures `device_ipv6` on its network interface and is now a fully participating node with a deterministic, registry-free address.

| Transport | Typical device class |
|---|---|
| IPv6 + HTTPS | Raspberry Pi, industrial gateway |
| IPv6 + CoAP | ESP32, Nordic nRF |
| LoRaWAN (IPv6 mapped) | LoRa nodes, agricultural sensors |
| BLE (RFC 7668) | Wearables, beacons |
| 6LoWPAN | Mesh sensor networks |

---

## Internal Algebraic Model

The following is the internal correctness model. Operators do not need to understand this to deploy. Implementers and conformance test authors should.

| Building analogy | Protocol concept | Operational meaning |
|---|---|---|
| chamber | fully typed resolved result at a stage | service result |
| apartment | coherent resolution path through all stages | request lifecycle |
| panel | stage boundary | contract boundary |
| wall | reject surface | validation failure |
| retraction | normalization onto canonical representative | canonicalization |
| residue | local service neighborhood | provider-specific subgraph |
| building | federation of typed services | semantic service mesh |

The stage progression:

```
Realized < Closed < Normalized < Projected
```

is the typed incidence structure. Each stage transition is a lawful rewrite step. The calculus guarantees that any two paths through the rewrite system that reach the same stage produce the same result (Confluence). Normalization is idempotent. Projection is read-only.

---

## Adoption Framing

**Sell the guarantees, not the geometry.**

| Guarantee | What operators get |
|---|---|
| Deterministic results | Same call → same result, on any conforming node |
| Registry-free discovery | Descriptors are derivable from SIDs without a registry |
| Canonical identity | Identity is stable across node migration and key rotation |
| Portable adapters | One SID produces EVM address, DID, IPv6, Git key — all from meaning |
| Replayable semantics | Every result is reproducible from the canonical call alone |

The building structure is used only to ensure the protocol remains coherent as it scales. It is not the adoption surface.

---

## Quick Reference: What Each Role Needs

### Operator

```
node binary or container
schema set (pinned digest)
artifact store
descriptor store
capability governor (root or delegated)
```

### Provider

```
schema definition
service SID (derived from normalized descriptor)
descriptor (published at GET /sid/{digest})
optional: adapter credentials (EVM, DID, IPv6)
```

### Broker

```
descriptor cache (epoch-aware)
capability verifier
SR-ABI proxy / router
```

### Consumer

```
SID of target service
stage request (Realized | Closed | Normalized | Projected)
SRCall constructor
```

### End-user client

```
GET /sid/{digest} → render IdentityDescriptor
POST /resolve (Projected) → render result
```

---

## Reading Order

**To deploy a single node:** DBC-1.2 → DBC-IDL-1.2 → DBC-NODE-1.0 → Profile A above  
**To build a broker:** DBC-NODE-1.0 §5–6 → Profile B above  
**To join a federation:** DBC-NODE-1.0 §6 → HD-RPC-1.0 §7 → Profile C above  
**To deploy a semantic IPv6 node:** HD-RPC-1.0 §5–6–8 → IPv6 section above  
**To write conformance tests:** DBC-1.2 → DBC-1.2-FIXTURES  
**To understand the full architecture:** HD-RPC-1.0 → this document

---

*HD-RPC-1.0 · Service Architecture and Deployment Profiles · Brian Thorne · bthornemail*
