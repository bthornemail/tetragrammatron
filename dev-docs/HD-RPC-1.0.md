# HD-RPC

**HD-RPC-1.0 · Architectural Specification**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Draft Specification · Audience: architects, node operators, ecosystem integrators  
Depends on: DBC-1.2, DBC-IDL-1.2, DBC-NODE-1.0

---

## Abstract

HD-RPC (Hierarchical Deterministic Remote Procedure Call) is a deterministic remote procedure call architecture in which procedure addresses are canonical semantic identities derived from normalized meaning and provenance.

In traditional RPC, a procedure is named by a string or URL. In HD-RPC, a procedure is named by a **SID** — a content-derived identifier produced from a canonical normal form under a fixed schema. The same meaning always produces the same address. The same address always resolves against the same calculus.

> **A semantic internet where meaning deterministically produces identity, identity produces addresses, and addresses produce computation.**

This document is the umbrella specification. It ties together the three existing layers — DBC-1.2 (computation), DBC-IDL-1.2 (identity), and DBC-NODE-1.0 (runtime) — into a complete, named architecture, and adds the IPv6 routing adapter that makes the system physically routable without registries.

---

## 0. Canonical Vocabulary

| Term | Meaning |
|---|---|
| HD-RPC | Hierarchical Deterministic Remote Procedure Call — this architecture |
| SID | Semantic Identity Digest — the canonical address of a procedure or entity |
| SR-ABI | Stage-indexed Resolution ABI — the wire protocol of HD-RPC calls |
| SRCall | A canonical HD-RPC request envelope |
| SRResult | A canonical HD-RPC response envelope |
| semantic address | A SID used as a procedure endpoint |
| semantic routing | Routing by SID-derived network address rather than by DNS name or location |
| adapter:ipv6 | The network routing adapter that derives a 128-bit IPv6 address from a SID |
| HD derivation | The property that nested semantic structures produce nested SIDs — analogous to HD wallet derivation, but over meaning rather than keys |

---

## Document Structure

| Section | Role |
|---|---|
| §1 — Core Principle | The single architectural inversion that defines HD-RPC |
| §2 — The Five Layers | How DBC-1.2, DBC-IDL-1.2, and DBC-NODE-1.0 compose into HD-RPC |
| §3 — Procedure Namespace | How semantic structures become a callable namespace |
| §4 — The HD-RPC Call | The SRCall as the canonical RPC unit |
| §5 — The IPv6 Adapter | Semantic routing via `adapter:ipv6` |
| §6 — Semantic Routing | How SID-derived addresses eliminate DNS and registries from routing |
| §7 — Federation | How multiple nodes form a semantic network |
| §8 — Device Participation | How constrained devices join the HD-RPC network |
| §9 — HD-RPC Laws | The invariants of the full architecture |
| §10 — Comparison | HD-RPC against REST, gRPC, Ethereum RPC, DID |
| §11 — Summary | Full stack, doctrine, and acceptance criteria |

---

## 1. Core Principle

Traditional RPC names a procedure by a location or string:

```
POST https://api.example.com/service/method
```

This couples the procedure identity to a server, a DNS name, and a URL structure. None of those are properties of the procedure itself.

HD-RPC inverts this:

```
traditional: endpoint → code
HD-RPC:      meaning → identity → computation
```

A procedure in HD-RPC is addressed by what it **is**, not where it **lives**.

```
call(SID, stage)
```

where `SID` is derived from the canonical normal form of the procedure's semantic descriptor under a fixed schema.

This has three consequences:

**Consequence 1 — Determinism.** The same meaning always produces the same address. There is no registration step, no namespace authority, no DNS lookup.

**Consequence 2 — Portability.** The procedure can move between nodes. Its address does not change because its address is its meaning, not its location.

**Consequence 3 — Verifiability.** Any party can independently derive the SID of a procedure from its published descriptor and schema. There is no trust anchor required for address derivation.

---

## 2. The Five Layers

HD-RPC is composed of five layers. The first four are defined in existing specs. The fifth is the HD-RPC binding defined here.

```
┌─────────────────────────────────────────────────┐
│  Layer 5 — Ecosystem Adapters                   │
│  wallets · DID · Git · IPFS · IPv6 · devices    │
├─────────────────────────────────────────────────┤
│  Layer 4 — Node Protocol   (DBC-NODE-1.0)       │
│  /resolve · /sid · /verify-capability · /adapter│
├─────────────────────────────────────────────────┤
│  Layer 3 — Semantic Identity   (DBC-IDL-1.2)    │
│  SID · CapabilityGrant · AdapterCredential      │
│  IdentityDescriptor · IdentityProjection        │
├─────────────────────────────────────────────────┤
│  Layer 2 — Resolution Calculus   (DBC-1.2)      │
│  realize · close · normalize · project          │
│  SR-ABI · SRCall · SRResult                     │
├─────────────────────────────────────────────────┤
│  Layer 1 — Transport                            │
│  HTTPS · libp2p · LoRa · BLE · IPv6 · socket   │
└─────────────────────────────────────────────────┘
```

### Layer 1 — Transport

HD-RPC is transport-agnostic. Any transport that can carry JSON payloads is admissible. The transport is responsible only for delivering `SRCall` envelopes to nodes and returning `SRResult` envelopes to callers.

| Transport | Use case |
|---|---|
| HTTPS | Internet-connected nodes |
| libp2p | Peer-to-peer federation |
| IPv6 raw socket | Semantic mesh routing |
| LoRa / 802.15.4 | Low-power device mesh |
| BLE | Local device-to-device |
| Unix socket | Same-host IPC |

No transport is privileged. The semantic address is the SID, not the transport endpoint.

### Layer 2 — Resolution Calculus (DBC-1.2)

The calculus defines the computation model: what it means to resolve a document against a schema through the stages `Realized → Closed → Normalized → Projected`. The SR-ABI defines the canonical wire encoding of calls and results.

This layer guarantees: **same canonical input → same canonical output**. Cross-node determinism follows directly from this guarantee.

### Layer 3 — Semantic Identity (DBC-IDL-1.2)

The identity layer derives SIDs from normal forms, delegates authority through capability grants, produces ecosystem credentials through adapters, publishes resolution surfaces through identity descriptors, and presents identity through projection.

This layer guarantees: **same meaning → same identity**. The semantic address is stable as long as the meaning is stable, independent of key rotation, node migration, or provider change.

### Layer 4 — Node Protocol (DBC-NODE-1.0)

The node protocol binds layers 2 and 3 to HTTP endpoints. A conforming node exposes exactly four endpoints. Any agent that can make HTTP requests can participate in HD-RPC.

### Layer 5 — Ecosystem Adapters (DBC-IDL-1.2 §3 + this spec §5)

Adapters derive ecosystem-specific credentials from SIDs via domain-separated HKDF. The `adapter:ipv6` adapter defined in §5 of this spec adds network routing as a first-class adapter domain.

---

## 3. Procedure Namespace

In HD-RPC the procedure namespace is a **SID tree** derived from the hierarchical structure of semantic descriptors.

### 3.1 Flat Address

A single semantic entity has a single SID:

```
SID(entity) = H(schema_digest, normalize(entity), federation_scope, context)
```

### 3.2 Hierarchical Derivation

Nested semantic structures produce nested SIDs. Given:

```
organization
  └─ repository
       └─ module
            └─ function
```

normalization produces structured normal forms at each level. Each level has its own SID:

```
SID(org)
SID(org/repo)           = H(..., normalize(repo | org), ...)
SID(org/repo/module)    = H(..., normalize(module | repo | org), ...)
SID(org/repo/module/fn) = H(..., normalize(fn | module | repo | org), ...)
```

This is **HD derivation** — hierarchical deterministic derivation of addresses from structure, exactly analogous to HD wallet derivation trees (BIP32) but derived from meaning rather than from keys.

The derivation is:
- **deterministic** — same structure → same SID tree
- **non-reversible** — a child SID does not reveal its parent's normal form
- **schema-pinned** — the tree is specific to the schema under which the structures were normalized

### 3.3 Callable at Every Level

Every node in the SID tree is independently callable:

```
call(SID(org/repo/module/fn), Normalized)
```

calls the function directly.

```
call(SID(org/repo), Normalized)
```

resolves the repository descriptor, from which the module and function SIDs can be derived by the caller.

### 3.4 Namespace Without a Registrar

The procedure namespace requires no registrar. Any party that knows the schema and the semantic descriptor can derive the correct SID independently. Two parties deriving the SID of the same function under the same schema will always arrive at the same address.

This is the first time a global procedure namespace has been achievable without a central registry.

---

## 4. The HD-RPC Call

### 4.1 The Canonical Call

An HD-RPC call is an `SRCall` envelope submitted to `POST /resolve` on a node that recognizes the relevant schema.

```json
{
  "abi_version":    "SR-ABI",
  "canonical_input": { ... },
  "document_digest": "sha256:<digest>",
  "null_digest":     "sha256:<digest>",
  "schema_digest":   "sha256:<digest>",
  "target_stage":    "Normalized",
  "view_digest":     "sha256:<digest>"
}
```

The node runs `resolveTo(target_stage, canonical_input)` and returns an `SRResult`.

### 4.2 Call Resolution Pipeline

```
Agent
  │  SRCall
  ▼
Node
  │  realize(σ, D)
  ▼
RealizedStructure
  │  close(σ, S)
  ▼
ClosedStructure
  │  normalize(σ, S)
  ▼
NormalForm
  │  [optional] project(v, N)
  ▼
ProjectionModel
  │  SRResult
  ▼
Agent
```

Stages are strictly ordered: `Realized < Closed < Normalized < Projected`. A call to `target_stage = Normalized` runs all stages up to and including normalization.

### 4.3 Determinism Guarantee

The same `SRCall` submitted to any conforming node in the same federation must produce the same `SRResult`. This is guaranteed by:

- DBC-1.2 Confluence law: normalization is confluent
- DBC-1.2 Idempotence laws: closure and normalization are idempotent
- SR-ABI Canonical Encoding: the wire encoding is deterministic
- DBC-NODE-1.0 Calculus Faithfulness: nodes must not introduce nondeterminism

This determinism guarantee is what makes HD-RPC **hierarchical deterministic**: not only are addresses derived deterministically from meaning, but the computation at those addresses is itself deterministic.

### 4.4 Authorized Calls

If the called procedure requires a capability grant, the caller must include a valid `CapabilityGrant` and present it to `POST /verify-capability` before submitting the `SRCall`. The node may require capability verification as a precondition for accepting certain calls, as declared in the procedure's `IdentityDescriptor`.

---

## 5. The IPv6 Adapter

### 5.1 Definition

The `adapter:ipv6` adapter derives a deterministic 128-bit IPv6 address from a SID.

```
adapter_label = "adapter:ipv6"
seed = HKDF(SID_bytes, "adapter:ipv6", length=16)
ipv6 = seed[0:16]  (128 bits, interpreted as a raw IPv6 address)
```

The HKDF derivation follows the same domain-separation discipline as all other adapters in DBC-IDL-1.2 §3.2.

### 5.2 IPv6 Address Form

The derived address is a full 128-bit IPv6 address. It should be declared in the ULA (Unique Local Address) range or in a federation-assigned prefix range to avoid collision with globally routed addresses:

```
fc00::/7    — ULA range (recommended for private federations)
2001:db8::/32 — documentation range (examples only)
```

For public federations, a federation operator may register a /48 prefix and map the lower 80 bits to the SID-derived suffix.

Example derived address:

```
fd3f:9a2b:1c91:f47a:9c4e:2a1b:f390:7c14
```

This address uniquely identifies a semantic entity within its IPv6 routing domain without any DHCP assignment, registry lookup, or DNS resolution.

### 5.3 Why IPv6 Is the Correct Routing Adapter

IPv6 was designed with the following assumptions:

- 128-bit address space (sufficient for per-entity addressing)
- stateless address autoconfiguration (SLAAC)
- mesh routing support
- no mandatory central registry for link-local or ULA addresses

These assumptions align exactly with the HD-RPC identity model. The SID-derived IPv6 address gives every semantic entity a **deterministic network presence** using the existing IPv6 routing infrastructure.

### 5.4 Node Autoconfiguration

A node computes its own IPv6 address without DHCP:

```
node_sid    = SID(normalize(node_descriptor), schema, federation_scope)
node_ipv6   = adapter(node_sid, "adapter:ipv6")
```

The node configures this address on its network interface and begins accepting connections. No registration is required. No DNS entry is needed.

This is zero-configuration semantic node deployment.

### 5.5 Service Discovery via IPv6

Given a SID, an agent can derive the service's IPv6 address and connect directly:

```
SID(service)
  │
  ▼  adapter:ipv6
  │
  ▼
IPv6 address
  │
  ▼
TCP/UDP connect
  │
  ▼
POST /resolve
```

The service is addressed by its meaning. Its physical location is irrelevant.

For services that expose additional endpoints (not just `/resolve`), the `IdentityDescriptor` at `GET /sid/{digest}` provides the full service endpoint list. The IPv6 address is the routing handle; the descriptor is the service directory.

### 5.6 Federation Address Layout

For federations that want structured address allocation, the following layout is recommended:

```
| 48 bits          | 16 bits    | 64 bits         |
  federation prefix  schema tag   SID hash suffix
```

This allows routers to forward federation traffic to the correct cluster without resolving the full SID.

The schema tag is the lower 16 bits of the schema digest. The SID hash suffix is the lower 64 bits of the full SID. This layout is advisory; implementations may use a flat `hash(SID)[0:16]` layout if structured allocation is not needed.

### 5.7 Multi-Transport Nodes

Nodes may be reachable over multiple transports simultaneously. The `IdentityDescriptor` for the node SID publishes all available transport endpoints:

```json
{
  "services": [
    { "type": "hd-rpc-https", "serviceEndpoint": "https://..." },
    { "type": "hd-rpc-ipv6",  "serviceEndpoint": "fd3f:9a2b:...:7c14" },
    { "type": "hd-rpc-lora",  "serviceEndpoint": "lora:<eui64>" }
  ]
}
```

Agents choose the transport appropriate to their environment. The procedure address (SID) is the same regardless of transport.

### 5.8 Adapter Law Addition

The `adapter:ipv6` adapter is governed by the same laws as all DBC-IDL-1.2 adapters:

- **Isolation:** `adapter(SID, "adapter:ipv6")` is computationally independent of `adapter(SID, "adapter:evm")`
- **Non-Reversal:** the IPv6 address does not reveal the SID
- **Determinism:** same SID → same IPv6 address

Additionally:

- **Non-Authority:** the IPv6 address is a routing handle. It does not define the SID, authorize any capability, or influence any layer above the transport.

---

## 6. Semantic Routing

### 6.1 What Semantic Routing Means

In conventional networking, routing is by location: a packet is forwarded toward a physical or administrative address. In HD-RPC, routing is by meaning: a packet is forwarded toward the node that hosts the canonical artifact for a given SID.

```
conventional: destination = location
HD-RPC:       destination = SID → IPv6
```

The network does not need to understand SIDs. It routes IPv6 packets normally. The semantic layer sits above routing: the agent derives the IPv6 address from the SID, and the network delivers the packet to that address.

### 6.2 Comparison with Existing Discovery Systems

| System | Discovery mechanism | Requires registry? |
|---|---|---|
| DNS | Name → IP lookup via registrar | Yes |
| Ethereum | ENS or hardcoded bootnodes | Yes |
| IPFS | DHT (Kademlia) | No, but probabilistic |
| DID | Method-specific resolution | Method-dependent |
| HD-RPC | SID → IPv6 derivation | No |

HD-RPC discovery is **deterministic and registry-free** because the address is derived from the identity, and the identity is derived from the meaning.

### 6.3 The Routing Stack

```
meaning (normal form)
  │
  ▼  DBC-1.2 normalize
  │
  ▼
NormalForm
  │
  ▼  DBC-IDL-1.2 SID derivation
  │
  ▼
SID
  │
  ▼  adapter:ipv6
  │
  ▼
IPv6 address
  │
  ▼  IPv6 routing (existing infrastructure)
  │
  ▼
Node
  │
  ▼  DBC-NODE-1.0 POST /resolve
  │
  ▼
SRResult
```

The IPv6 infrastructure is a passive carrier. The semantic layer does not require it to be upgraded. Standard IPv6 routing delivers packets to the correct node.

---

## 7. Federation

### 7.1 What Makes a Federation

A federation is a set of HD-RPC nodes that share:

```
schema set        (same admitted schemas)
federation scope  (same FIDX scope)
trust anchors     (same root governors)
epoch definition  (same time/round boundary)
```

Because the calculus is deterministic and normalization is confluent, all honest nodes in a federation will derive the same SID for any given document and schema. No consensus about identity is required — only consensus about which schemas are admitted.

### 7.2 The Federation Property (formal)

Given honest nodes N1 and N2 in the same federation, with the same schema set σ and the same document D:

```
normalize(σ, realize(σ, D)) = N   (DBC-1.2 confluence)
SID(N, σ, scope, context)   = s   (DBC-IDL-1.2 determinism)
```

Then N1 and N2 agree on `s` without communicating. Identity is **ambient** in a shared calculus.

### 7.3 Geometric Interpretation

A DBC federation of honest nodes is a distributed realization of a single canonical apartment in the sense of buildings (Tits, Bruhat-Tits). The shared calculus defines the building. The schema set defines the type. Each node hosts a chamber. Normalization is retraction onto the apartment. Confluence is the intersection property.

This means: **federated HD-RPC nodes are not merely technically convergent — they are geometrically convergent in the sense that their shared calculus traces a path through a canonical chamber complex.**

---

## 8. Device Participation

### 8.1 Minimal Node

A device can participate as an HD-RPC node if it can:

1. compute SHA-256 or BLAKE3 (for SID derivation)
2. run HKDF (for adapter derivation)
3. serve the four node endpoints over at least one transport

A constrained device (ESP32, Raspberry Pi, LoRa gateway) satisfies all three requirements.

### 8.2 Device SID and Address

A device derives its own SID from its normalized hardware descriptor:

```
device_sid  = SID(normalize(device_descriptor), schema, federation_scope)
device_ipv6 = adapter(device_sid, "adapter:ipv6")
```

The device configures `device_ipv6` on its network interface. It is now a fully participating HD-RPC node with a deterministic, registry-free address.

### 8.3 Physical–Semantic Bridge

Hardware devices represent physical objects. When a device's hardware descriptor is normalized into the blackboard, the device enters the same semantic namespace as software modules, agents, documents, and services. The identity model is uniform across physical and virtual entities:

```
SID(person)
SID(device)
SID(world/scene)
SID(repository/module)
SID(service/endpoint)
```

All are callble semantic addresses in the same HD-RPC namespace. This is the **cyber-physical semantic network**: the semantic internet extends to hardware without a separate IoT identity layer.

### 8.4 Supported Transports for Constrained Devices

| Transport | Typical device class |
|---|---|
| IPv6 + HTTPS | Raspberry Pi, industrial gateway |
| IPv6 + CoAP | ESP32, Nordic nRF |
| LoRaWAN (IPv6 mapped) | LoRa nodes, agricultural sensors |
| BLE (IPv6 over BLE, RFC 7668) | Wearables, beacons |
| 802.15.4 / 6LoWPAN | Mesh sensor networks |

---

## 9. HD-RPC Laws

These are the invariants that a conforming HD-RPC system must maintain. They are additive to the laws of DBC-1.2, DBC-IDL-1.2, and DBC-NODE-1.0.

| Law | Statement |
|---|---|
| Semantic Address | Every callable procedure has a SID derived from its canonical normal form |
| HD Derivation | Nested semantic structures produce nested SIDs deterministically |
| Call Determinism | Same SRCall → same SRResult on any conforming node in the same federation |
| Transport Neutrality | The SID is invariant across transports; transport choice does not change the semantic address |
| IPv6 Non-Authority | A SID-derived IPv6 address is a routing handle; it does not define the SID or authorize any capability |
| Namespace Without Registrar | Any party can derive the correct SID from the schema and descriptor without consulting a registry |
| Federation Convergence | Honest nodes in a federation converge on the same SID for the same document and schema |
| Physical Uniformity | Device descriptors, software modules, and agent descriptors all inhabit the same SID namespace |

---

## 10. Comparison

### 10.1 Against REST

| Property | REST | HD-RPC |
|---|---|---|
| Procedure address | URL string | SID (content-derived) |
| Namespace authority | DNS registrar | None — derived from meaning |
| Determinism | No — depends on server implementation | Yes — calculus guaranteed |
| Semantic binding | None | Full — address = meaning |
| Location coupling | Yes — URL encodes server location | No — SID is location-independent |
| Discovery | DNS + documentation | IdentityDescriptor at GET /sid |

### 10.2 Against gRPC

| Property | gRPC | HD-RPC |
|---|---|---|
| Procedure address | Package + service + method strings | SID |
| Schema definition | Protobuf IDL | Normalized blackboard schema |
| Determinism | No | Yes |
| Cross-language | Via protobuf codegen | Via canonical JSON + SR-ABI |
| Streaming | Yes | Not in 1.0 |

### 10.3 Against Ethereum RPC

| Property | Ethereum RPC | HD-RPC |
|---|---|---|
| Procedure address | Contract address + ABI selector | SID |
| Address derivation | Key pair | Meaning |
| Determinism | Yes (EVM) | Yes (DBC calculus) |
| Global registry | Yes (blockchain) | No |
| Identity | Key-first | Meaning-first |
| Ecosystem adapters | EVM native only | EVM + DID + Git + IPFS + IPv6 + … |

### 10.4 Against W3C DID

| Property | DID | HD-RPC |
|---|---|---|
| Identity source | Key or method-specific | Canonical meaning |
| Discovery | Method-specific registry | IdentityDescriptor (no registry) |
| Procedure call | Not defined | SRCall / SRResult |
| Semantic binding | None | Full |
| Ecosystem compatibility | DID methods | `did:dbc:` projection + all other adapters |

---

## 11. Summary

### 11.1 The Full Architecture

```
meaning (normal form)
  │
  ▼  DBC-1.2
  │
NormalForm
  │
  ▼  DBC-IDL-1.2
  │
SID
├── CapabilityGrant    [who may act]
├── AdapterCredential  [ecosystem form]
│   ├── adapter:evm
│   ├── adapter:did:key
│   ├── adapter:ipv6   ← [network address]
│   └── …
├── IdentityDescriptor [resolution surface]
└── IdentityProjection [presentation]
  │
  ▼  DBC-NODE-1.0
  │
Node
├── POST /resolve
├── GET  /sid/{digest}
├── POST /verify-capability
└── GET  /adapter/{label}/{sid}
  │
  ▼  IPv6 / HTTPS / LoRa / BLE / …
  │
Ecosystem
```

### 11.2 The Doctrine

```
Identity is canonical.
Authority is delegated.
Credentials are adapters.
Descriptors are discoverable.
Presentation is projection.
Procedures are addressed by meaning.
```

### 11.3 The Laws

| Law | Defined in |
|---|---|
| Stage Soundness | DBC-1.2 |
| Confluence | DBC-1.2 |
| SID Determinism | DBC-IDL-1.2 |
| Descriptor Non-Authority | DBC-IDL-1.2 |
| Calculus Faithfulness | DBC-NODE-1.0 |
| Semantic Address | HD-RPC-1.0 §9 |
| HD Derivation | HD-RPC-1.0 §9 |
| Call Determinism | HD-RPC-1.0 §9 |
| Transport Neutrality | HD-RPC-1.0 §9 |
| IPv6 Non-Authority | HD-RPC-1.0 §9 |
| Namespace Without Registrar | HD-RPC-1.0 §9 |
| Federation Convergence | HD-RPC-1.0 §9 |
| Physical Uniformity | HD-RPC-1.0 §9 |

### 11.4 Acceptance Criteria

A conforming HD-RPC implementation must satisfy all acceptance criteria of DBC-1.2, DBC-IDL-1.2, and DBC-NODE-1.0, and additionally:

- Procedure addresses are SIDs derived from canonical normal forms under a fixed schema
- Nested semantic structures produce nested SIDs deterministically (HD derivation)
- The same SRCall produces the same SRResult on any conforming node in the same federation
- The transport used to deliver an SRCall does not affect the SID or the SRResult
- `adapter:ipv6` derivation follows DBC-IDL-1.2 §3.2 HKDF discipline with domain label `"adapter:ipv6"`
- IPv6 addresses derived from SIDs are used only as routing handles; they are never authoritative over SID derivation or capability grants
- Any party can independently derive the correct SID from the schema and semantic descriptor without contacting a registry
- Device descriptors, software module descriptors, and agent descriptors may all participate in the same SID namespace

---

## Changelog

| Version | Change |
|---|---|
| HD-RPC-1.0 | Initial specification. Defines HD-RPC as the umbrella architecture over DBC-1.2, DBC-IDL-1.2, and DBC-NODE-1.0. Adds `adapter:ipv6` (§5), semantic routing (§6), HD derivation namespace (§3), federation convergence property (§7), device participation model (§8), and the HD-RPC law inventory (§9). |

---

*HD-RPC-1.0 · Hierarchical Deterministic Remote Procedure Call · Brian Thorne · bthornemail*
