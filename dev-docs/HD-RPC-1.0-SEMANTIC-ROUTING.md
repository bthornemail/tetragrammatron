# HD-RPC-1.0 · Semantic Routing and Circular Closure

**Author:** Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
**Status:** Draft · Addendum to HD-RPC-1.0  
**Depends on:** DBC-1.2 · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0

---

## The Core Inversion

Traditional architectures route by location:

```
connect to server
```

HD-RPC routes by meaning:

```
connect to meaning
```

The addition of `adapter:ipv6` makes this physically true. The network itself becomes semantically addressable. The full chain becomes:

```
meaning → identity → address → computation
```

---

## The IPv6 Adapter

IPv6 is 128 bits. A SID digest is 256 bits. The routing address is derived from the SID via the same HKDF discipline used by all other adapter domains:

```
seed = HKDF(
  ikm    = sid_bytes,
  salt   = federation_scope_bytes,
  info   = "adapter:ipv6",
  length = 16
)
ipv6_address = fd00::/8 prefix + seed[0:15]
```

`adapter:ipv6` is structurally identical to `adapter:evm`, `adapter:did:key`, `adapter:bip39`. It is a deterministic projection of identity onto a domain. The domain happens to be the network routing layer.

**IPv6 Non-Authority law:** the derived IPv6 address is a routing handle only. It does not define the SID, does not authorize any capability, and is not a substitute for SID verification.

---

## Why IPv6 Is the Right Domain

IPv6 already assumes:

- 128-bit addresses
- stateless autoconfiguration (SLAAC)
- mesh routing
- no central allocation required for link-local and ULA ranges

These properties align exactly with the HD-RPC philosophy. `SID → IPv6` gives global routing without registries — the same guarantee that SID gives for identity.

---

## Zero-Configuration Nodes

A new node appears. It computes:

```
SID(normalize(node_descriptor), schema, federation_scope)
```

Then derives:

```
ipv6 = adapter(SID, "adapter:ipv6")
```

The node configures `ipv6` on its network interface. It now has a deterministic, globally-unique, registry-free address. No DHCP. No DNS. No registration.

Discovery follows the same pattern. Any agent that knows the SID of a service can derive its IPv6 address and connect directly:

```
SID(service)
    │
    ▼
adapter:ipv6
    │
    ▼
IPv6 address
    │
    ▼
POST /resolve
```

---

## HD-RPC Call Flow with IPv6

```
Agent
  │  knows SID(service)
  ▼
adapter:ipv6  →  IPv6 address
  │
  ▼
connect (HTTPS / CoAP / raw socket)
  │
  ▼
POST /resolve  (SRCall)
  │
  ▼
Node: realize → close → normalize → project
  │
  ▼
SRResult  →  Agent
```

The service is addressed by meaning. The IP address is derived, not registered. The result is canonical.

---

## Federation Routing

Federation scope is already embedded in SID derivation:

```
SID = H(schema_digest, normal_form, federation_scope, context)
```

Because `federation_scope` is an input to both SID derivation and IPv6 adapter derivation, nodes in the same federation derive compatible addresses. Nodes in different federations derive non-overlapping address spaces without any coordination.

No routing protocol needs to understand semantics. Routing works on the derived IPv6 addresses. Semantics live in the SID space above the network layer.

---

## Device Participation

Any device that can compute SHA-256, run HKDF, and serve the four node endpoints over at least one transport is a full HD-RPC node.

A device derives its SID from its normalized hardware descriptor:

```
device_sid  = SID(normalize(device_descriptor), schema, federation_scope)
device_ipv6 = adapter(device_sid, "adapter:ipv6")
```

The device configures `device_ipv6` on its network interface. Physical objects — sensors, actuators, gateways — now inhabit the same semantic namespace as software modules, agents, and services. The identity model is uniform:

```
SID(person)
SID(device)
SID(world/scene)
SID(repository/module)
SID(service/endpoint)
```

All are callable semantic addresses. All can derive IPv6 routing handles. The cyber-physical boundary dissolves.

| Transport | Device class |
|---|---|
| IPv6 + HTTPS | Raspberry Pi, industrial gateway |
| IPv6 + CoAP | ESP32, Nordic nRF |
| LoRaWAN (IPv6 mapped) | LoRa nodes, agricultural sensors |
| BLE (RFC 7668) | Wearables, beacons |
| 6LoWPAN | Mesh sensor networks |

---

## The Full Stack

```
meaning (normal form)
        │
        ▼  DBC-1.2
NormalForm
        │
        ▼  DBC-IDL-1.2
SID
  ├── CapabilityGrant     [who may act]
  ├── AdapterCredential
  │     ├── adapter:evm
  │     ├── adapter:did:key
  │     ├── adapter:ipv6   ← network address
  │     └── …
  ├── IdentityDescriptor  [resolution surface]
  └── IdentityProjection  [presentation]
        │
        ▼  DBC-NODE-1.0
Node
  ├── POST /resolve
  ├── GET  /sid/{digest}
  ├── POST /verify-capability
  └── GET  /adapter/{label}/{sid}
        │
        ▼  IPv6 / HTTPS / LoRa / BLE / …
Ecosystem
```

---

## Circular Closure

The semantic computation loop closes because the output of resolution is again a NormalForm — which feeds back into identity derivation.

```
meaning
   ↓
identity
   ↓
authority
   ↓
address
   ↓
computation
   ↓
meaning
```

Most systems are not closed loops. Their meaning is external — held in documentation, human convention, or mutable registry state.

| System | Structure | Closure |
|---|---|---|
| REST | endpoint → code → database | open — meaning is external |
| Ethereum | key → address → contract → state | open — meaning is external |
| IPFS | content → hash | partial — no computation semantics |
| HD-RPC | meaning → identity → address → computation → meaning | closed |

The closure is not incidental. It follows from the Confluence and Normalization Idempotence laws of DBC-1.2:

```
resolve(normalize(S)) = normalize(resolve(S))
```

Computation and canonicalization commute. This is what allows distributed nodes to converge without a consensus protocol — convergence is a mathematical consequence of the calculus, not a coordination mechanism imposed on top of it.

---

## The Commuting Square

```
      S  ──normalize──▶  NormalForm(S)
      │                       │
    resolve                 resolve
      │                       │
      ▼                       ▼
  Result(S)  ──normalize──▶  NormalForm(Result(S))
```

Any path through the square produces the same canonical result. This is the algebraic property that makes HD-RPC federation coherent: nodes can execute in any order, cache intermediate results, and still converge.

---

## Why the Geometric Structures Keep Appearing

The recurring appearance of Fano planes, Moufang planes, octonions, and the 240-cell in this design space is not accidental. These structures arise whenever a system has:

- local operations with consistent rewrite rules
- global symmetry from canonical normalization
- canonical closure — outputs return to the same normal form space as inputs

HD-RPC has exactly these properties. The geometry is the internal structure of the algebra. The protocol is its operational surface.

---

## One-Sentence Description

> **A semantic internet where meaning deterministically produces identity, identity produces addresses, and addresses produce computation — and computation returns to meaning.**

---

## Discovery Without Registries

Most decentralized systems solve discovery with external anchors:

| System | Discovery mechanism |
|---|---|
| Ethereum | bootnodes (hardcoded) |
| IPFS | DHT |
| DNS | registries |
| HD-RPC | `identity → address` — deterministic from SID |

When identity and address are the same derivation, discovery collapses into derivation. The question "where is this service?" has the same answer as "what is this service?" — both are answered by computing the SID and applying the adapter.

---

## Addendum: SID Space as Semantic Coordinate Space

The earlier Universal IP Basis intuition —

```
PATH.length / N = ±{0..K}
```

— was expressing addresses as algebraic coordinate systems. In HD-RPC that becomes:

```
SID space = semantic coordinate space
IPv6      = projection of that space onto network routing
```

A hash space already provides the coordinate structure: deterministic, collision-resistant, uniformly distributed. The IPv6 adapter is simply the projection map from the 256-bit semantic space onto the 128-bit routing space.

The architecture does not abandon the geometric intuition. It implements it.

---

*HD-RPC-1.0 · Semantic Routing and Circular Closure · Brian Thorne · bthornemail*
