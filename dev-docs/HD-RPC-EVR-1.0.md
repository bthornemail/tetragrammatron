# HD-RPC-EVR-1.0 · Canonical Event Registry

**HD-RPC-EVR-1.0 · Specification**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Draft Specification · Audience: node implementers, broker authors, GUI implementers, federation operators  
Depends on: DBC-1.2 · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0

---

## Abstract

The HD-RPC Event Registry defines a canonical, versioned taxonomy of event kinds emitted by nodes, brokers, providers, consumers, devices, and agents participating in the HD-RPC stack. It is the common operational language for what happened in the system.

The Event Registry follows the same architectural principle as DBC:

> **EventKind determines evidence shape.**

This mirrors the DBC law:

> **Requested stage determines codomain.**

Each event kind has a fixed semantic meaning, a fixed canonical payload shape, fixed actor/subject/routing fields, and fixed evidence requirements. An event that does not carry the required evidence for its kind is not a conforming event.

The Event Registry is not a mutable log schema invented independently by each implementation. It is a governed taxonomy. Implementations emit events by kind; they do not invent new shapes.

---

## 0. Canonical Vocabulary

| Term | Meaning |
|---|---|
| event | a canonical, typed record of something that happened in the HD-RPC stack |
| event kind | the string identifier of an event class, e.g. `resolve.succeeded` |
| event family | a group of related event kinds sharing a layer boundary |
| event envelope | the canonical JSON wrapper around every event |
| evidence | the required payload fields that prove the event occurred |
| causal fields | digests linking the event to the canonical artifacts it references |
| `evr:hdrpc:` | the event ID prefix namespace for HD-RPC events |
| emitter | the node, broker, device, or agent that emits an event |
| subscriber | the GUI workspace, monitoring system, or operator tool that receives events |

---

## Document Structure

| Section | Role |
|---|---|
| §1 — Event Envelope | The canonical wrapper for all events |
| §2 — Event Families | The seven families and their event kinds |
| §3 — Event Definitions | Full definition for each event kind |
| §4 — Evidence Rules | What constitutes a valid event |
| §5 — Emitter Obligations | Which components must emit which events |
| §6 — Subscriber Model | How events are consumed |
| §7 — Event Registry Laws | The invariants a conforming implementation must maintain |
| §8 — Minimal Initial Set | The smallest deployable event surface |
| §9 — Acceptance Criteria | What a conforming implementation must satisfy |

---

## 1. Event Envelope

Every event in the HD-RPC stack uses the following canonical envelope. All fields are required. Fields with no applicable value use `null` for scalars and `{}` for objects.

```json
{
  "actor_sid":        "sid:dbc:<digest> | null",
  "causal":           {},
  "event_id":         "evr:hdrpc:<sha256_of_envelope_minus_event_id>",
  "event_kind":       "<family>.<kind>",
  "event_version":    "EVR-1.0",
  "federation_scope": "<federation_name> | null",
  "node_sid":         "sid:dbc:<digest>",
  "payload":          {},
  "schema_digest":    "sha256:<digest> | null",
  "subject_sid":      "sid:dbc:<digest> | null",
  "timestamp_epoch":  "<epoch_identifier>"
}
```

### Field Definitions

| Field | Required | Meaning |
|---|---|---|
| `event_id` | always | SHA-256 of the canonical envelope with `event_id` set to `null`, encoded as `evr:hdrpc:<hex>` |
| `event_kind` | always | dot-separated `family.kind` identifier from the registry |
| `event_version` | always | `"EVR-1.0"` — version of this registry |
| `timestamp_epoch` | always | the epoch in which the event occurred |
| `node_sid` | always | the SID of the node that emitted the event |
| `subject_sid` | when applicable | the SID the event is about |
| `actor_sid` | when applicable | the SID initiating the action that produced this event |
| `federation_scope` | when applicable | the federation scope of the node |
| `schema_digest` | when applicable | the schema under which the relevant artifact was produced |
| `causal` | always | digests linking this event to relevant canonical artifacts |
| `payload` | always | event-kind-specific data fields |

### Causal Fields

The `causal` object links the event to the canonical artifacts it references. All causal field values are SHA-256 digests prefixed `sha256:` or `null`.

| Causal field | Meaning |
|---|---|
| `call_digest` | digest of the `SRCall` that triggered this event |
| `result_digest` | digest of the `SRResult` produced |
| `descriptor_digest` | digest of the `IdentityDescriptor` involved |
| `capability_digest` | digest of the `CapabilityGrant` involved |
| `governor_chain_digest` | digest of the full governor chain presented |
| `revocation_digest` | digest of the revocation record, if applicable |

Not all events use all causal fields. Each event kind definition specifies which causal fields are required.

---

## 2. Event Families

The registry is organized into seven families, one per layer boundary in the HD-RPC stack.

| Family | Layer | Emitter |
|---|---|---|
| A — Resolution | DBC-1.2 / SR-ABI | node |
| B — Identity | DBC-IDL-1.2 / SID + Descriptor | node |
| C — Capability | DBC-IDL-1.2 / CapabilityGrant | node, broker |
| D — Adapter | DBC-IDL-1.2 / AdapterCredential | node |
| E — Routing | HD-RPC-1.0 / adapter:ipv6 | node, device, broker |
| F — Federation | HD-RPC-1.0 / federation protocol | node, federation operator |
| G — Cyber-physical | HD-RPC-1.0 §8 / device participation | agent, device, node |

---

## 3. Event Definitions

### Family A — Resolution

Resolution events track the SR-ABI calculus lifecycle at the node boundary.

---

#### `resolve.received`

Emitted when a node accepts a valid `SRCall` for processing.

| Field | Value |
|---|---|
| `subject_sid` | null (not yet derived) |
| `actor_sid` | null (not required at receipt) |
| Required causal | `call_digest` |
| Payload | `target_stage`, `schema_digest` |

```json
{
  "causal":    { "call_digest": "sha256:..." },
  "payload":   { "schema_digest": "sha256:...", "target_stage": "Normalized" }
}
```

---

#### `resolve.succeeded`

Emitted when the calculus reaches the requested stage and returns a canonical `SRResult`.

| Field | Value |
|---|---|
| `subject_sid` | SID derived from the produced NormalForm, if `target_stage = Normalized` |
| Required causal | `call_digest`, `result_digest` |
| Payload | `stage`, `value_kind`, `value_digest` |

```json
{
  "causal":  { "call_digest": "sha256:...", "result_digest": "sha256:..." },
  "payload": { "stage": "Normalized", "value_digest": "sha256:...", "value_kind": "NormalForm" }
}
```

---

#### `resolve.rejected`

Emitted when the calculus returns a typed `SRResult` reject.

| Field | Value |
|---|---|
| Required causal | `call_digest` |
| Payload | `reject_kind`, `reject_code`, `stage` |

```json
{
  "causal":  { "call_digest": "sha256:..." },
  "payload": { "reject_code": "invalid_symbol", "reject_kind": "RejectRealize", "stage": "Realized" }
}
```

---

#### `resolve.protocol_error`

Emitted when the node returns an HTTP 400 or 422 — the request did not reach the calculus.

| Field | Value |
|---|---|
| Required causal | none |
| Payload | `http_status`, `error_class` (`malformed_envelope` \| `unknown_schema`) |

---

#### `resolve.cached`

Emitted when the node returns a cached `SRResult` for an identical prior `SRCall`.

| Field | Value |
|---|---|
| Required causal | `call_digest`, `result_digest` |
| Payload | `stage`, `value_kind` |

---

### Family B — Identity

Identity events track the SID derivation and descriptor lifecycle.

---

#### `identity.derived`

Emitted when a SID is derived from a NormalForm.

| Field | Value |
|---|---|
| `subject_sid` | the derived SID |
| Required causal | `result_digest` (the NormalForm that was hashed) |
| Payload | `schema_digest`, `federation_scope` |

---

#### `descriptor.projected`

Emitted when an `IdentityDescriptor` is projected from governed state and stored.

| Field | Value |
|---|---|
| `subject_sid` | the SID the descriptor describes |
| Required causal | `descriptor_digest` |
| Payload | `epoch`, `governor_count`, `adapter_labels` |

```json
{
  "causal":  { "descriptor_digest": "sha256:..." },
  "payload": { "adapter_labels": ["adapter:evm", "adapter:did:key", "adapter:ipv6"], "epoch": "...", "governor_count": 1 }
}
```

---

#### `descriptor.served`

Emitted when `GET /sid/{digest}` returns a descriptor to a caller.

| Field | Value |
|---|---|
| `subject_sid` | the SID requested |
| Required causal | `descriptor_digest` |
| Payload | `format` (`native` \| `did`), `epoch` |

---

#### `descriptor.stale`

Emitted when the node detects that a served descriptor's governed state has changed.

| Field | Value |
|---|---|
| `subject_sid` | the stale SID |
| Required causal | `descriptor_digest` |
| Payload | `stale_epoch`, `current_epoch` |

---

#### `descriptor.not_found`

Emitted when `GET /sid/{digest}` returns HTTP 404.

| Field | Value |
|---|---|
| Required causal | none |
| Payload | `sid_digest_requested`, `federation_fallback_attempted` (bool) |

---

### Family C — Capability

Capability events track the authority lifecycle.

---

#### `capability.presented`

Emitted when a capability grant is submitted to `POST /verify-capability`.

| Field | Value |
|---|---|
| `subject_sid` | `capability_grant.subject_sid` |
| `actor_sid` | `capability_grant.actor_sid` |
| Required causal | `capability_digest` |
| Payload | `scope`, `epoch` |

---

#### `capability.valid`

Emitted when all five verification steps pass.

| Field | Value |
|---|---|
| `subject_sid` | grant's `subject_sid` |
| `actor_sid` | grant's `actor_sid` |
| Required causal | `capability_digest`, `governor_chain_digest` |
| Payload | `scope`, `rights`, `epoch` |

---

#### `capability.expired`

Emitted when verification fails at step 1 (epoch check).

| Field | Value |
|---|---|
| Required causal | `capability_digest` |
| Payload | `expired_epoch`, `current_epoch` |

---

#### `capability.revoked`

Emitted when a revocation record is found for this grant.

| Field | Value |
|---|---|
| Required causal | `capability_digest`, `revocation_digest` |
| Payload | `revocation_epoch` |

---

#### `capability.invalid_signature`

Emitted when the governor signature does not verify.

| Field | Value |
|---|---|
| Required causal | `capability_digest` |
| Payload | `governor_sid` |

---

#### `capability.broken_chain`

Emitted when the governor chain does not trace to a root trust anchor.

| Field | Value |
|---|---|
| Required causal | `capability_digest` |
| Payload | `chain_depth_reached`, `expected_root_sid` |

---

#### `capability.schema_mismatch`

Emitted when the grant applies to a different schema than this node recognizes.

| Field | Value |
|---|---|
| Required causal | `capability_digest` |
| Payload | `grant_schema_digest`, `node_schema_digest` |

---

### Family D — Adapter

Adapter events track ecosystem credential derivation.

---

#### `adapter.derived`

Emitted when `GET /adapter/{label}/{sid}` successfully derives a credential.

| Field | Value |
|---|---|
| `subject_sid` | the SID the credential was derived from |
| Required causal | none (credential is the evidence) |
| Payload | `adapter_label`, `credential_kind` (NOT the credential value for sensitive adapters) |

---

#### `adapter.denied`

Emitted when a sensitive adapter credential is requested without a valid capability grant.

| Field | Value |
|---|---|
| `subject_sid` | the SID requested |
| `actor_sid` | the requesting actor |
| Required causal | none |
| Payload | `adapter_label`, `denial_reason` |

---

#### `adapter.unknown`

Emitted when the requested adapter label is not in this node's adapter registry.

| Field | Value |
|---|---|
| Required causal | none |
| Payload | `adapter_label_requested` |

---

#### `adapter.public_served`

Emitted when a public adapter credential (EVM address, DID, Git ref, IPFS CID, IPv6) is served.

| Field | Value |
|---|---|
| `subject_sid` | the SID |
| Payload | `adapter_label`, `credential_kind` |

---

#### `adapter.private_withheld`

Emitted when a sensitive adapter (BIP39, device private key) is requested but the actor has insufficient capability.

| Field | Value |
|---|---|
| Required causal | `capability_digest` (the grant that was insufficient) |
| Payload | `adapter_label`, `required_scope` |

---

### Family E — Routing

Routing events track semantic routing via `adapter:ipv6`.

---

#### `route.derived`

Emitted when a SID-to-IPv6 derivation is computed.

| Field | Value |
|---|---|
| `subject_sid` | the SID |
| Payload | `ipv6_address`, `federation_scope` |

---

#### `route.connected`

Emitted when a node successfully connects to a peer via a derived IPv6 address.

| Field | Value |
|---|---|
| `subject_sid` | the peer node SID |
| Payload | `ipv6_address`, `transport` |

---

#### `route.unreachable`

Emitted when a connection attempt to a derived IPv6 address fails.

| Field | Value |
|---|---|
| `subject_sid` | the target SID |
| Payload | `ipv6_address`, `transport`, `failure_reason` |

---

#### `route.peer_discovered`

Emitted when a federation announcement is received from a new peer.

| Field | Value |
|---|---|
| `subject_sid` | the announcing peer's node SID |
| Payload | `peer_endpoint`, `peer_ipv6`, `epoch` |

---

#### `route.federation_mismatch`

Emitted when a peer announcement arrives with a different `federation_scope` than this node's.

| Field | Value |
|---|---|
| `subject_sid` | the mismatched peer SID |
| Payload | `peer_federation_scope`, `local_federation_scope` |

---

### Family F — Federation

Federation events track multi-node convergence.

---

#### `federation.joined`

Emitted when a node successfully joins a federation (its SID appears in a peer's peer list).

| Field | Value |
|---|---|
| `subject_sid` | this node's SID |
| Payload | `federation_scope`, `schema_digest`, `epoch` |

---

#### `federation.peer_added`

Emitted when a new peer is added to this node's peer list.

| Field | Value |
|---|---|
| `subject_sid` | the new peer's node SID |
| Payload | `peer_endpoint`, `schema_digest` |

---

#### `federation.schema_mismatch`

Emitted when a peer presents an `SRCall` with a `schema_digest` not in this node's schema set.

| Field | Value |
|---|---|
| `subject_sid` | the peer SID |
| Required causal | `call_digest` |
| Payload | `peer_schema_digest`, `admitted_schema_digests` |

---

#### `federation.epoch_advanced`

Emitted when the federation epoch advances.

| Field | Value |
|---|---|
| Payload | `prior_epoch`, `new_epoch` |

---

#### `federation.replay_converged`

Emitted when a cross-runtime replay test (R-01 or R-05 from DBC-TEST-MATRIX) produces identical results on independent nodes.

| Field | Value |
|---|---|
| Required causal | `call_digest`, `result_digest` |
| Payload | `test_id`, `runtime_a_sid`, `runtime_b_sid` |

---

#### `federation.replay_diverged`

Emitted when a replay test produces differing results — a conformance violation.

| Field | Value |
|---|---|
| Required causal | `call_digest` |
| Payload | `test_id`, `runtime_a_result_digest`, `runtime_b_result_digest`, `divergence_stage` |

This event is a critical conformance signal. `federation.replay_diverged` must never occur on a correctly implemented federation.

---

### Family G — Cyber-physical

Cyber-physical events track the bootstrap and operation of device and agent nodes.

---

#### `device.descriptor_normalized`

Emitted when a hardware descriptor is successfully normalized through the calculus.

| Field | Value |
|---|---|
| Required causal | `call_digest`, `result_digest` |
| Payload | `device_class`, `transport` |

---

#### `device.identity_derived`

Emitted when a device SID is derived from its normalized descriptor.

| Field | Value |
|---|---|
| `subject_sid` | the derived device SID |
| Required causal | `result_digest` |
| Payload | `device_class`, `federation_scope` |

---

#### `device.address_configured`

Emitted when the device derives its IPv6 address and configures it on its network interface.

| Field | Value |
|---|---|
| `subject_sid` | the device SID |
| Payload | `ipv6_address`, `transport` |

---

#### `device.capability_bound`

Emitted when a capability grant is verified for the device.

| Field | Value |
|---|---|
| `subject_sid` | the device SID |
| Required causal | `capability_digest` |
| Payload | `scope`, `governor_sid`, `epoch` |

---

#### `device.published`

Emitted when the device's `IdentityDescriptor` is projected and made discoverable.

| Field | Value |
|---|---|
| `subject_sid` | the device SID |
| Required causal | `descriptor_digest` |
| Payload | `epoch`, `services_count` |

---

#### `device.bootstrapped`

Composite event emitted after all seven bootstrap steps complete successfully. This is the single event that confirms a device is a fully participating HD-RPC node.

| Field | Value |
|---|---|
| `subject_sid` | the device SID |
| Required causal | `descriptor_digest`, `capability_digest` |
| Payload | `ipv6_address`, `transport`, `federation_scope`, `epoch` |

```json
{
  "causal": {
    "capability_digest":  "sha256:...",
    "descriptor_digest":  "sha256:..."
  },
  "payload": {
    "epoch":            "...",
    "federation_scope": "...",
    "ipv6_address":     "fd3f:9a2b:...",
    "transport":        "ipv6+coap"
  },
  "subject_sid": "sid:dbc:<digest>"
}
```

---

#### `device.surface_projected`

Emitted when a device's sensor or control surface is rendered as a projection.

| Field | Value |
|---|---|
| `subject_sid` | the device SID |
| Payload | `surface_class`, `projection_digest` |

---

#### `device.telemetry_ingested`

Emitted when telemetry from a device is submitted to the calculus as a document and resolved.

| Field | Value |
|---|---|
| `subject_sid` | the device SID |
| Required causal | `call_digest`, `result_digest` |
| Payload | `telemetry_kind`, `stage` |

---

#### `device.command_projected`

Emitted when a command is projected to a device surface via `POST /resolve` at stage Projected.

| Field | Value |
|---|---|
| `subject_sid` | the device SID |
| Required causal | `call_digest`, `result_digest` |
| Payload | `command_kind`, `surface_class` |

---

## 4. Evidence Rules

An event is conforming only when:

- it carries all required causal fields for its kind (specified in §3)
- its `event_id` matches `evr:hdrpc:` + SHA-256 of the canonical envelope with `event_id = null`
- its `event_kind` is a registered kind from this registry
- its `event_version` is `"EVR-1.0"`
- its `payload` contains all fields specified for its kind

An event that does not carry required evidence for its kind must be rejected by conforming subscribers.

**EventKind determines evidence shape.** A `resolve.succeeded` event without a `result_digest` in `causal` is not a conforming `resolve.succeeded` event.

---

## 5. Emitter Obligations

| Component | Must emit |
|---|---|
| Node (any call) | `resolve.received`, `resolve.succeeded` or `resolve.rejected`, `resolve.protocol_error` |
| Node (NormalForm produced) | `identity.derived`, `descriptor.projected` |
| Node (descriptor served) | `descriptor.served` |
| Node (adapter endpoint) | `adapter.derived` or `adapter.denied` or `adapter.unknown` |
| Node (capability endpoint) | `capability.presented` + one of the capability result events |
| Node (IPv6 adapter) | `route.derived`, `adapter.public_served` |
| Node (federation) | `federation.joined`, `federation.peer_added`, `federation.epoch_advanced` |
| Broker | `capability.presented`, `capability.valid`, `route.connected`, `route.unreachable` |
| Agent / Device | all Family G events in bootstrap sequence order |
| Federation operator | `federation.replay_converged` or `federation.replay_diverged` after replay tests |

---

## 6. Subscriber Model

Events are delivered to subscribers. The EVR does not specify a transport — implementations may use WebSocket, SSE, NATS, Kafka, or any pub/sub mechanism that preserves event ordering within a SID scope.

**Subscription filters:**

| Filter | Meaning |
|---|---|
| `subject_sid` | receive all events about a given SID |
| `event_family` | receive all events in a family (e.g., all `resolve.*` events) |
| `event_kind` | receive a specific event kind only |
| `federation_scope` | receive all events within a federation |
| `node_sid` | receive all events from a given node |

Filters may be combined. A GUI workspace typically subscribes to one or two families filtered by `subject_sid` or `federation_scope`.

**Subscriber laws:**

- Subscribers are read-only. They observe; they do not emit.
- A subscriber that receives a non-conforming event (missing required evidence) must discard it and may log a warning.
- Subscribers must never use event data as input to identity, capability, or adapter derivation. Events are observational.

---

## 7. Event Registry Laws

| Law | Statement |
|---|---|
| EventKind Determines Evidence Shape | Each event kind has a fixed canonical payload and fixed required causal fields. Non-conforming events are rejected. |
| Event Identity | `event_id = evr:hdrpc:H(canonical_envelope_with_event_id_null)`. No two distinct events may share an event_id. |
| Event Immutability | Once emitted, an event is never modified. Events are append-only. |
| Emitter Completeness | Every required event for a given operation must be emitted. Partial emission is a conformance violation. |
| Observer Non-Authority | Events describe what happened. They are never authoritative over canonical state. An event's `subject_sid` is a reference, not a definition. |
| No Backflow | Event data must not be used as input to calculus, identity, or capability layers. |
| Divergence Signal | `federation.replay_diverged` must never occur on a correctly implemented federation. Its occurrence is a critical conformance signal requiring investigation. |

---

## 8. Minimal Event Registry

The smallest event surface sufficient for a first deployable conformance set:

| Event kind | Why required |
|---|---|
| `resolve.succeeded` | proves calculus faithfulness |
| `resolve.rejected` | proves reject surface correctness |
| `identity.derived` | proves SID determinism |
| `descriptor.projected` | proves descriptor non-authority |
| `capability.valid` | proves capability delegation |
| `adapter.derived` | proves adapter determinism |
| `route.derived` | proves IPv6 adapter |
| `federation.replay_converged` | proves federation convergence |
| `device.bootstrapped` | proves cyber-physical participation |

---

## 9. Acceptance Criteria

A conforming HD-RPC-EVR-1.0 implementation must satisfy:

- All required events for each node endpoint are emitted (§5 Emitter Obligations)
- Every event carries the required causal fields for its kind
- `event_id` is correctly computed as specified in §1
- `event_version` is `"EVR-1.0"` on all events
- Events are append-only; no event is modified after emission
- `federation.replay_diverged` is treated as a critical conformance signal and does not occur in correct implementations
- Subscribers are read-only and discard non-conforming events
- Event data is never used as input to the calculus, identity, or capability layers

---

## Changelog

| Version | Change |
|---|---|
| HD-RPC-EVR-1.0 | Initial specification. Seven event families, 38 named event kinds, canonical envelope, evidence rules, emitter obligations, subscriber model, and seven event registry laws. |

---

*HD-RPC-EVR-1.0 · Canonical Event Registry · Brian Thorne · bthornemail*
