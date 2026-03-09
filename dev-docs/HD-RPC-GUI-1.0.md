# HD-RPC-GUI-1.0 · Role-Based Projection Shell

**HD-RPC-GUI-1.0 · Specification**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Draft Specification · Audience: GUI implementers, frontend architects, operator tooling authors  
Depends on: DBC-1.2 · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0 · HD-RPC-EVR-1.0

---

## Abstract

The HD-RPC projection shell is a role-based operator interface over canonical artifacts. It is not an application for editing state. It is a **projection surface** — a read-mostly, event-driven shell that observes, resolves, verifies, projects, and routes, and never mutates canonical truth directly.

Every workspace in the shell is backed by one or more of the four node endpoints. Every live view is powered by Event Registry subscriptions. The GUI adds no semantic authority. It is downstream of the calculus in exactly the same way that `IdentityProjection` is downstream of the SID.

> **The shell is a governed artifact observer. Operators see canonical truth; they do not author it.**

---

## 0. Canonical Vocabulary

| Term | Meaning |
|---|---|
| shell | the GUI as a whole — a projection surface over canonical artifacts |
| workspace | a top-level role-scoped view within the shell |
| panel | a focused view within a workspace |
| live feed | an event-driven panel updated by EVR subscriptions |
| primary action | the one canonical operation each role performs as its primary task |
| no-write rule | the shell invariant that canonical state is never mutated through the GUI directly |

---

## Document Structure

| Section | Role |
|---|---|
| §1 — Shell Doctrine | The invariants the shell must maintain |
| §2 — Role Map | The five operator roles and their primary actions |
| §3 — Information Architecture | The six top-level workspaces |
| §4 — Workspace Specifications | Full panel and backing endpoint definition for each workspace |
| §5 — Event Subscriptions | Which workspaces subscribe to which EVR event families |
| §6 — Screen Definitions | The five role-scoped console layouts |
| §7 — Shell Laws | The invariants a conforming shell implementation must maintain |
| §8 — Acceptance Criteria | What a conforming implementation must satisfy |

---

## 1. Shell Doctrine

The shell obeys the same projection doctrine as the rest of the HD-RPC stack:

```
observe
resolve
verify
project
route
```

and never:

```
mutate canonical truth directly
```

This follows from the DBC-1.2 No-Backflow rule (projection surfaces may not mutate canonical state), the DBC-NODE-1.0 No Semantic Invention law (nothing in the system introduces semantic content), and the HD-RPC-1.0 architecture principle that the stack flows in one direction: meaning → identity → address → computation → meaning.

The shell sits at the projection layer. It is the rightmost surface of the stack. It receives and presents; it does not originate.

**What this means in practice:**

- All writes enter the system through `POST /resolve` as canonical `SRCall` envelopes
- All identity views come from `GET /sid/{digest}`
- All authority views come from `POST /verify-capability`
- All routing and ecosystem views come from `GET /adapter/{label}/{sid}`
- All live operational timelines come from HD-RPC-EVR-1.0 subscriptions

The shell never constructs a semantic artifact from free-form input and writes it to an artifact store directly. If an operator wants to publish a new descriptor, they submit a document to `/resolve` and let the calculus produce the canonical artifact.

---

## 2. Role Map

Five operator roles. Each has a primary action, a primary workspace, and a narrow set of endpoints it primarily exercises.

| Role | Primary action | Primary workspace | Primary endpoints |
|---|---|---|---|
| Provider | publish meaning | Provider Console | `POST /resolve`, `GET /sid/{digest}` |
| Broker / Proxy | route identity | Broker Console | `GET /sid/{digest}`, `POST /verify-capability` |
| Consumer | request stages | Consumer Workbench | `POST /resolve` |
| End-user | receive projections | Projection Portal | `GET /sid/{digest}` (read-only) |
| Agent operator | bootstrap cyber-physical nodes | Agent / Device Ops | all four endpoints |

The role map mirrors the deployment doctrine:

```
Providers publish meaning.
Brokers route identity.
Consumers request stages.
Users receive projections.
```

---

## 3. Information Architecture

The shell has six top-level workspaces. Each workspace is scoped to a layer of the HD-RPC stack.

| Workspace | Stack layer | Primary role |
|---|---|---|
| A — Identity | DBC-IDL-1.2 (SID, descriptor) | Provider, Broker, Consumer |
| B — Resolution | DBC-1.2 (SR-ABI) | Consumer, Provider |
| C — Capabilities | DBC-IDL-1.2 (capability grants) | Broker, Provider, Agent |
| D — Routing | HD-RPC-1.0 (IPv6, federation routes) | Agent, Broker |
| E — Federation | DBC-NODE-1.0 §6, HD-RPC-1.0 §7 | Provider, Broker, Agent |
| F — Projections | DBC-IDL-1.2 §5, HD-RPC-1.0 §3 | End-user, Agent |

Each workspace is visible to the roles that need it. The Projection Portal (end-user role) exposes only workspace F. The Provider Console exposes A, B, E, and a limited slice of C. The Broker Console exposes A, C, D, and E. The Consumer Workbench exposes B and A. The Agent / Device Ops console exposes all six.

---

## 4. Workspace Specifications

### Workspace A — Identity

**Purpose:** inspect semantic identity, not keys.

**Backing endpoints:**
- `GET /sid/{digest}` — retrieves the `IdentityDescriptor`
- `GET /adapter/{label}/{sid}` — retrieves ecosystem credentials

**Panels:**

| Panel | Contents | Source |
|---|---|---|
| SID / FIDX viewer | `sid:dbc:` string, `fidx:` string if federation-scoped | `/sid/{digest}` |
| Schema digest | `schema_digest` field | `/sid/{digest}` |
| Federation scope | `federation_scope` field | `/sid/{digest}` |
| Derivation context | `derivation_context` fields | `/sid/{digest}` |
| Identity descriptor | full `IdentityDescriptor` JSON, pretty-printed | `/sid/{digest}` |
| Governor set | `governors[]` — SID + verification method for each | `/sid/{digest}` |
| Adapter bindings | `adapters{}` — label → credential value | `/sid/{digest}` |
| Epoch validity | `epoch` field + current epoch comparison | `/sid/{digest}` |
| DID projection | W3C DID document derived from descriptor | `/sid/{digest}?format=did` |

**Live feed:** subscribes to `identity.derived`, `descriptor.projected`, `descriptor.served`, `descriptor.stale`.

**Shell law enforced:** Identity workspace panels are read-only. The `IdentityDescriptor` is served; it is never edited in the shell.

---

### Workspace B — Resolution

**Purpose:** issue canonical stage requests and inspect canonical results.

**Backing endpoints:**
- `POST /resolve` — submits `SRCall`, receives `SRResult`

**Panels:**

| Panel | Contents | Source |
|---|---|---|
| SRCall editor | structured editor for `canonical_input`, `schema_digest`, `target_stage`, `view_digest` | user input → `POST /resolve` |
| Stage selector | four-stage radio: `Realized \| Closed \| Normalized \| Projected` | drives `target_stage` in SRCall |
| Canonical digest preview | computed `document_digest`, `schema_digest` before submission | local computation |
| SRResult viewer | formatted `canonical_value`, `value_kind`, `stage`, `value_digest` | `POST /resolve` response |
| Reject evidence viewer | formatted `reject_kind`, `reject_code`, `canonical_evidence` on failure | `POST /resolve` response |
| History | recent SRCall / SRResult pairs, keyed by `document_digest + schema_digest + stage` | local cache |

**Live feed:** subscribes to `resolve.received`, `resolve.succeeded`, `resolve.rejected`, `resolve.cached`.

**Shell law enforced:** The SRCall editor constructs a valid canonical envelope and submits it to `/resolve`. It does not write directly to an artifact store.

---

### Workspace C — Capabilities

**Purpose:** inspect authority state — who may act for which SID, under what constraints.

**Backing endpoints:**
- `POST /verify-capability` — verifies a capability grant chain

**Panels:**

| Panel | Contents | Source |
|---|---|---|
| Capability chain explorer | visual graph: `subject_sid → governor_sid → actor_sid` | `POST /verify-capability` |
| Governor chain path | chain from root trust anchor to current governor | `/verify-capability` response |
| Epoch validity | `epoch` field + time-to-expiry display | `/verify-capability` response |
| Revocation status | `revocation.revocation_digest`, last-checked timestamp | `/verify-capability` response |
| Actor / subject matrix | tabular view: which actors hold grants for which subjects | operator-composed from multiple `/verify-capability` calls |
| Verification result | `valid \| expired \| revoked \| invalid_signature \| broken_chain \| schema_mismatch` | `/verify-capability` response |

**Live feed:** subscribes to `capability.presented`, `capability.valid`, `capability.expired`, `capability.revoked`, `capability.broken_chain`.

**Shell law enforced:** Capability workspace panels display verification results. They never issue capability grants. Grant issuance is an out-of-shell governor action.

---

### Workspace D — Routing

**Purpose:** semantic routing visibility — SID-to-address derivation, federation topology, device reachability.

**Backing endpoints:**
- `GET /adapter/ipv6/{sid}` — derives IPv6 routing handle
- `GET /sid/{digest}` — retrieves `services[]` endpoints from descriptor

**Panels:**

| Panel | Contents | Source |
|---|---|---|
| SID → IPv6 derivation | input SID, display derived `fd...` address | `/adapter/ipv6/{sid}` |
| Federation route table | known SIDs, their derived IPv6 addresses, liveness status | local federation peer list + liveness check |
| Peer discovery | announced peers from `POST /federation/announce`, shown as SID + IPv6 | federation announcements |
| Device class / transport map | device SIDs grouped by transport: HTTPS / CoAP / LoRa / BLE / 6LoWPAN | `/sid/{digest}` services field |
| Network liveness | reachability status for each known SID address | background probe |
| Multi-transport view | for a given SID, all published transport endpoints from its descriptor | `/sid/{digest}` services field |

**Live feed:** subscribes to `route.derived`, `route.connected`, `route.unreachable`, `route.peer_discovered`, `route.federation_mismatch`.

**Shell law enforced:** Routing workspace derives addresses from SIDs. It never assigns addresses. No routing entry overrides SID derivation.

---

### Workspace E — Federation

**Purpose:** schema set management, node convergence, epoch coordination.

**Panels:**

| Panel | Contents | Source |
|---|---|---|
| Schema set | pinned schema digests admitted by this federation | node configuration |
| Peer list | federation peers: `node_sid`, `endpoint`, `epoch` | `GET /federation/peers` |
| Federation scope | `federation_scope` identifier | node configuration |
| Convergence / replay health | pass/fail status for `R-01`, `R-05` replay tests | DBC-TEST-MATRIX integration |
| Node identity map | each node's SID, IPv6, transport, liveness | peer list + liveness |
| Epoch coordination | current epoch, time-to-advance, peer epoch agreement | federation epoch state |

**Live feed:** subscribes to `federation.joined`, `federation.peer_added`, `federation.schema_mismatch`, `federation.epoch_advanced`, `federation.replay_converged`, `federation.replay_diverged`.

**Shell law enforced:** Schema sets are read-only in the shell. Schema additions require out-of-shell node re-initialization per DBC-NODE-1.0 Schema Closure law.

---

### Workspace F — Projections

**Purpose:** end-user and device surface display. The only workspace most end-users should see.

**Backing endpoints:**
- `GET /sid/{digest}` — identity and service cards
- `POST /resolve` with `target_stage: Projected` — projected results

**Panels:**

| Panel | Contents | Source |
|---|---|---|
| Service card | SID, display name from descriptor, federation scope, verified status | `/sid/{digest}` |
| World / scene / device surface | projected scene or device state | `POST /resolve` (Projected stage) |
| Event timeline | chronological stream of events relevant to this SID | EVR subscription |
| Projected identity | `IdentityProjection` — display-ready identity card | `/sid/{digest}` + adapter credentials |
| Projected results | `ProjectionModel` rendered per `SurfaceClass` | `POST /resolve` (Projected stage) |

**Live feed:** subscribes to `projection.rendered`, `projection.failed`, `device.surface_projected`.

**Shell law enforced:** Projection workspace is strictly read-only. No-backflow applies: nothing displayed here may be used as input to identity, capability, or adapter layers.

---

## 5. Event Subscriptions

Each workspace subscribes to a defined set of EVR event families. The event stream drives live feeds without polling.

| Workspace | EVR families subscribed |
|---|---|
| A — Identity | Identity, Descriptor |
| B — Resolution | Resolution |
| C — Capabilities | Capability |
| D — Routing | Routing |
| E — Federation | Federation |
| F — Projections | Projection, Cyber-physical (device surface events) |

Subscriptions are read-only. The shell never emits events into the EVR stream. Events flow from nodes and brokers to the shell.

---

## 6. Screen Definitions

Each role-scoped console is a curated subset of the six workspaces, with a primary action button that initiates the role's canonical operation.

### Provider Console

**Primary action:** submit document to `/resolve` → publish canonical artifact

**Workspaces exposed:** A (Identity), B (Resolution), E (Federation), C (Capabilities — read-only)

**Tabs:**

| Tab | Workspace | Contents |
|---|---|---|
| Schemas | E | admitted schema digests, schema pinning status |
| Published Services | A | SIDs published by this provider, descriptor status |
| SIDs | A | SID viewer, FIDX viewer |
| Descriptors | A | descriptor JSON, epoch, governor set |
| Adapters | A | adapter bindings for each published SID |
| Event Stream | all | live feed filtered to this provider's SIDs |

---

### Broker Console

**Primary action:** verify capability → route SRCall to provider node

**Workspaces exposed:** A (Identity), C (Capabilities), D (Routing), E (Federation)

**Tabs:**

| Tab | Workspace | Contents |
|---|---|---|
| Descriptor Cache | A | cached descriptors with epoch validity |
| Capability Verification | C | verification results, chain explorer |
| Route Table | D | SID → IPv6 → liveness for all known providers |
| Forwarded Calls | B | recent SRCall / SRResult pairs proxied by this broker |
| Federation Peers | E | peer list, node identity map |
| Event Stream | all | live feed of routing, capability, and federation events |

**Shell law enforced:** Broker Console never redefines semantics. It routes. `No Semantic Invention` applies.

---

### Consumer Workbench

**Primary action:** construct SRCall → inspect SRResult

**Workspaces exposed:** B (Resolution), A (Identity — read-only)

**Tabs:**

| Tab | Workspace | Contents |
|---|---|---|
| SID Resolver | A | look up SID → descriptor → available services |
| Stage Runner | B | stage selector + one-click resolve |
| SRCall Editor | B | full canonical SRCall editor |
| SRResult Viewer | B | structured result + digest verification |
| Reject Inspector | B | reject evidence + reject family taxonomy |
| Event Timeline | B | resolve events for this consumer's calls |

---

### User Projection Portal

**Primary action:** receive projection

**Workspaces exposed:** F (Projections) only

**Tabs:**

| Tab | Workspace | Contents |
|---|---|---|
| Service Cards | F | verified service identities |
| Verified Identities | F | SID-backed identity display |
| Current Results | F | projected results for subscribed services |
| Device / World Surfaces | F | cyber-physical projection surfaces |
| Notifications | F | event timeline filtered to user-relevant events |

**Shell law enforced:** Projection Portal is strictly read-only. No operator action is available. Users observe; they do not act.

---

### Agent / Device Ops

**Primary action:** bootstrap device → derive SID + IPv6 → join federation

**Workspaces exposed:** all six

**Tabs:**

| Tab | Workspace | Contents |
|---|---|---|
| Bootstrap Queue | B + D | devices awaiting descriptor normalization |
| Device Descriptor | B | descriptor editor → submit to `/resolve` |
| SID + IPv6 | A + D | derived SID, derived IPv6, address configuration |
| Capability State | C | device capability chain, governor, epoch |
| Peer Discovery | D + E | federation peers, routing table |
| Telemetry | F | device event timeline, sensor surfaces |
| Event Timeline | all | full event stream for this device |

**Bootstrap sequence steps** (driven by this console, event-verified):

```
Step 1 — normalize device descriptor   → resolve.succeeded, device.descriptor_normalized
Step 2 — derive identity               → identity.derived, device.identity_derived
Step 3 — derive routing address        → route.derived, device.address_configured
Step 4 — bind authority                → capability.valid, device.capability_bound
Step 5 — publish descriptor            → descriptor.projected, device.published
Step 6 — join federation               → federation.joined, route.peer_discovered
Step 7 — begin projections             → projection.rendered, device.surface_projected
```

---

## 7. Shell Laws

A conforming shell implementation must maintain all of the following invariants.

| Law | Statement |
|---|---|
| No Direct Mutation | The shell never writes canonical artifacts directly. All writes enter through `POST /resolve`. |
| Endpoint Fidelity | Every data panel is backed by the exact node endpoint specified. No data is fabricated by the shell. |
| No-Backflow | Projection workspace outputs are never used as inputs to identity, capability, or adapter layers. |
| Read-Only Projections | Workspace F and the User Projection Portal expose no write operations. |
| Role Scoping | Each console exposes only the workspaces defined for its role. Provider Console does not expose Projection Portal panels. |
| Event-Driven Live Feeds | Live feeds are driven by EVR subscriptions, not by polling the node endpoints. |
| No Semantic Invention | The shell produces no semantic content. It presents what nodes serve. |
| Schema Closure Respect | Schema set panels are read-only. Schema additions require out-of-shell node re-initialization. |
| Digest Verification | Displayed descriptors and artifacts include their canonical digest. The shell verifies the digest before display and marks unverified content visibly. |

---

## 8. Acceptance Criteria

A conforming HD-RPC-GUI-1.0 implementation must satisfy:

- All writes are submitted as canonical `SRCall` envelopes to `POST /resolve`
- Identity panels retrieve data from `GET /sid/{digest}` only
- Capability panels retrieve data from `POST /verify-capability` only
- Routing panels derive IPv6 addresses via `GET /adapter/ipv6/{sid}` only
- No panel displays fabricated semantic content
- Projection workspace and User Projection Portal expose no write operations
- Live feeds are event-driven via EVR subscriptions, not polling
- Descriptor panels display `descriptor_digest` and mark verification status
- Agent / Device Ops bootstrap sequence emits the correct EVR events at each step
- Broker Console never rewrites SRCall or SRResult payloads in transit

---

## Changelog

| Version | Change |
|---|---|
| HD-RPC-GUI-1.0 | Initial specification |

---

*HD-RPC-GUI-1.0 · Role-Based Projection Shell · Brian Thorne · bthornemail*
