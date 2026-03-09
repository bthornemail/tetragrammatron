# Tetragrammatron · Roadmap

**ROADMAP-1.0**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Working draft  
See also: [README.md](./README.md) · [ARCHITECTURE.md](./ARCHITECTURE.md)

---

## Phasing Principle

The roadmap follows the dependency order of the stack. Each phase must be complete and conformance-tested before the phase above it begins. No phase introduces scope from a higher layer.

```
Phase 1 — Substrate     NRR-1.0
Phase 2 — Protocol      DBC-1.2 · DBC-IDL-1.2
Phase 3 — Core          DBC-NODE-1.0
Phase 4 — Network       HD-RPC-1.0
Phase 5 — Hub           HD-RPC-GUI-1.0 · HD-RPC-EVR-1.0
Phase 6 — Ecosystem     adapters · fixtures · conformance
Phase 7 — Embedded      ESP32 · WASM · constrained runtimes
```

---

## Phase 1 — Substrate

**Target:** NRR-1.0 conformant runtime

**Deliverables:**

- `put / get / append / log` required API
- SHA-256 content addressing
- binary `log.bin` fixed-record format (37 bytes per entry)
- `objects/` blob store with two-level path derivation
- `boundary.bin` and `manifest.json`
- bundle export / import (atomic)
- full integrity verification pass
- NRR-CONFORMANCE-1.0 minimal test set: G-01..G-05 · N-01..N-08 · P-03 · R-01..R-02

**Acceptance gate:** `NRR-G-03` (replay from empty state produces expected final state) and `NRR-R-01` (checkpoint replay = full replay) pass on two independent language implementations.

**Out of scope for Phase 1:** schema law, SID derivation, federation, transport adapters, GUI.

---

## Phase 2 — Protocol

**Target:** DBC-1.2 and DBC-IDL-1.2 conformant reference implementation

**Deliverables:**

- realize → close → normalize → project pipeline
- SR-ABI envelope: `SRCall` and `SRResult`
- canonical reject families with typed evidence
- SID derivation: TLV encoding, domain tag `"DBC-SID-1.1\0"`, `sid:dbc:` prefix namespace
- `CapabilityGrant` with epoch and governor chain
- `AdapterCredential` derivation (public adapters: ipv6, evm, did:key, git)
- `IdentityDescriptor` projection and self-verification
- DBC-1.2-FIXTURES golden artifact set
- DBC-TEST-MATRIX: G-01..G-10 · N-01..N-10 (minimal initial set)

**Acceptance gate:** SID Determinism — same NormalForm produces the same SID on two independent runtimes. Commuting Square — `normalize(resolve(S)) = resolve(normalize(S))` for all golden fixture inputs.

**Out of scope for Phase 2:** HTTP node endpoints, federation, routing, GUI.

---

## Phase 3 — Core

**Target:** DBC-NODE-1.0 conformant node

**Deliverables:**

- `POST /resolve` — runs calculus, stores in NRR, returns SRResult
- `GET /sid/{digest}` — serves IdentityDescriptor; `?format=did` projection
- `POST /verify-capability` — five-step verification, six typed results
- `GET /adapter/{label}/{sid}` — public adapter derivation; sensitive adapter capability guard
- node SID derived from node's own normalized descriptor
- `POST /federation/announce` — descriptor propagation endpoint
- EVR event emission for all required node operations (per HD-RPC-EVR-1.0 §5)
- integration tests: I-01 (full pipeline), I-06 (node faithfulness), I-07 (federation convergence)

**Acceptance gate:** `POST /resolve` followed by `GET /sid/:digest` returns the descriptor whose `sid` matches `sid:dbc:<digest>` — end-to-end pipeline confirmed. Replay test R-01 passes on the node's NRR store.

**Out of scope for Phase 3:** HD-RPC semantic routing, federation fabric, GUI.

---

## Phase 4 — Network

**Target:** HD-RPC-1.0 conformant semantic routing and federation fabric

**Deliverables:**

- `call(SID, stage)` semantic addressing client
- HD namespace derivation: nested SID hierarchy
- `adapter:ipv6` — HKDF-based ULA address derivation from SID
- zero-config node bootstrap: SID → IPv6 → interface configuration
- broker/proxy forwarding (Deployment Profile B)
- multi-provider federation (Deployment Profile C)
- federation route table and peer discovery
- HD-RPC federation convergence tests

**Acceptance gate:** two independently initialized nodes with the same schema set and the same document produce the same SID and the same derived IPv6 address without coordination. `federation.replay_converged` event is emitted; `federation.replay_diverged` is never emitted.

**Out of scope for Phase 4:** GUI shell, EVR visualization, device bootstrap agents.

---

## Phase 5 — Hub

**Target:** HD-RPC-GUI-1.0 and HD-RPC-EVR-1.0 conformant projection shell

**Deliverables:**

- five role-scoped consoles: Provider · Broker · Consumer · User · Agent
- six workspaces backed by exact node endpoints per GUI-1.0 §4
- EVR SSE subscription feeds for each workspace
- No Direct Mutation shell law enforced in all write paths
- Digest Verification: descriptors displayed with verified digest status
- Agent / Device Ops bootstrap sequence: seven steps, event-verified
- EVR: all 38 event kinds with required evidence fields
- `federation.replay_diverged` treated as critical conformance signal

**Acceptance gate:** all nine GUI shell laws pass automated review. All five role consoles display only data from the four specified node endpoints. EVR event stream emits events in correct family/kind format with required causal fields.

**Out of scope for Phase 5:** embedded runtimes, adapter ecosystem expansion, multi-language conformance.

---

## Phase 6 — Ecosystem

**Target:** production-ready adapters, fixtures, and conformance infrastructure

**Deliverables:**

**tetragrammatron-adapters:**
- adapter:ipv6 — production HKDF derivation, 6LoWPAN support
- adapter:evm — EIP-55 checksummed address, signature verification
- adapter:did:key — W3C DID conformant, `did:dbc:` method registration
- adapter:git — remote URL derivation, federation over Git transport
- adapter:ipfs — CIDv1 derivation, IPFS pinning integration
- adapter:bip39 — sensitive; capability grant required, hardware wallet support
- adapter:oauth — OAuth 2.0 credential projection, token lifetime bounded by epoch

**tetragrammatron-fixtures:**
- golden NormalForms for all demo scenarios
- golden IdentityDescriptors
- golden EVR event streams for all seven families
- cross-runtime replay traces

**tetragrammatron-conformance:**
- complete DBC-CONFORMANCE-MATRIX: 70 laws → test IDs
- automated conformance runner
- multi-language test vector sharing (TypeScript, Rust, Go, Python)
- cross-runtime SID determinism verification harness

**Acceptance gate:** DBC-CONFORMANCE-MATRIX is 100% covered by executable tests. At least two language implementations pass the full conformance matrix.

---

## Phase 7 — Embedded

**Target:** NRR-1.0 embedded profile conformance on ESP32 and WASM

**Deliverables:**

**ESP32:**
- BLAKE3 hash (hardware-accelerated where available)
- NVS partition log or custom append-log on flash
- ring-buffer operation with valid checkpoint before wrap
- streaming blob mode (blobs fetched from peer, not stored locally)
- `device.bootstrapped` EVR event on successful seven-step bootstrap
- LoRa / BLE / CoAP transport adapters

**WASM:**
- `crypto.subtle.digest` for SHA-256
- OPFS or IndexedDB for blob store and log
- identical API surface as Node.js reference implementation
- browser projection shell integration

**Acceptance gate:** NRR-P-01 (same bundle replays identically on ESP32 and desktop runtime). `device.bootstrapped` event emitted with correct descriptor and IPv6 after clean power cycle.

---

## Deferred

The following are explicitly out of scope for all phases above, but noted for future consideration.

| Item | Reason deferred |
|---|---|
| Pfister / octonion doctrine layer | research-stage; not yet formally specified |
| Streaming SR-ABI support | requires extension to the SR-ABI envelope; deferred to DBC-1.3 |
| `adapter:bip39` hardware wallet integration | dependent on hardware security module spec |
| Full reject family: RejectClose | spec complete; test fixtures not yet published |
| Multi-epoch federation migration | requires epoch coordination protocol spec |
| `did:dbc:` DID method registry submission | blocked on DID method specification maturity |

---

## Version Policy

Specifications use semantic versioning scoped by layer.

- NRR-1.x patch releases are backward compatible (new optional API, same wire format)
- DBC-1.x patch releases are backward compatible (new reject codes, same SRCall/SRResult envelope)
- DBC-IDL-1.x patch releases are backward compatible (new adapters, same SID derivation)
- A change that modifies the TLV encoding, domain tag, or SID derivation algorithm requires a major version increment and is a breaking change for all SIDs in the ecosystem

---

## Contributing

The project follows the law-first contribution model:

1. Every new feature must be expressible as a named law
2. Every named law must have a corresponding conformance test
3. No feature may be merged until its layer's test matrix is green

The doctrine is fixed. New contributions add precision; they do not revise the nine doctrinal lines.

---

*Tetragrammatron · ROADMAP-1.0 · Brian Thorne · bthornemail*
