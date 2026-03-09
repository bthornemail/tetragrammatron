# Tetragrammatron · Demo

**Demo-1.0 · Projection Shell**

Demonstrates the full stack loop in ~500 LOC:

```
document → normalize → SID → NRR → resolve → project
```

---

## Stack demonstrated

| File | Layer | Spec |
|---|---|---|
| `src/nrr.js` | Substrate | NRR-1.0 |
| `src/dbc.js` | Protocol | DBC-1.2 |
| `src/idl.js` | Protocol | DBC-IDL-1.2 |
| `src/node.js` | Core | DBC-NODE-1.0 |
| `src/ui.html` | Hub | HD-RPC-GUI-1.0 |

---

## Run

```bash
node src/node.js
# open http://localhost:3000
```

No build step. No dependencies. Node 18+ required.

---

## What the demo proves

**Substrate:** NRR stores immutable artifacts under content-addressed refs.

**Protocol:** Normalization creates canonical meaning. `bob ≡ alice` → everything collapses to `alice`.

**Identity:** SID is derived from the NormalForm, not from keys. Same document always produces the same SID.

**Core:** Node runs the calculus, stores artifacts, serves descriptors.

**Hub:** UI projects canonical artifacts and the EVR event stream.

---

## Scenarios

| Scenario | Document | What it shows |
|---|---|---|
| Person card | `alias(bob,alice) + hasType(bob,Person)` | basic alias collapse |
| Org + member | org alias + multiple types | multi-node normalization |
| Device | sensor alias + Device type | same flow for IoT |
| Chain collapse | 3-alias chain + parentOf transitivity | transitive closure |

---

## Endpoints

```
POST /resolve              SR-ABI calculus
GET  /sid/:digest          IdentityDescriptor
GET  /adapter/:label/:sid  Adapter credential derivation
GET  /events               EVR event stream (SSE)
GET  /nrr/stats            NRR substrate stats
GET  /                     Projection shell UI
```

---

## What is NOT in demo 1

- capability chains (`/verify-capability`)
- federation and IPv6 adapter
- broker routing
- device bootstrap
- full DBC reject families
- Pfister/SIMD backend

Those are phase 2.

---

## Phase 2 plan

**Demo 2:** Add `Projected` stage with a view filter.

**Demo 3:** Add `adapter:ipv6` and a second node. Prove: same document → same SID → same IPv6 address on two independent nodes.

---

*Tetragrammatron · NRR-1.0 · DBC-1.2 · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-GUI-1.0*
