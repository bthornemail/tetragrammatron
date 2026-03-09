# DBC Blackboard Node Protocol

**DBC-NODE-1.0 · Specification**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Draft Specification · Audience: node operators, federation implementers, runtime authors  
Depends on: DBC-1.2, DBC-IDL-1.2

---

## Abstract

The DBC Blackboard Node Protocol defines the minimal operational surface that a conforming node must expose to participate in a DBC federation. It translates the computation guarantees of DBC-1.2 and the identity guarantees of DBC-IDL-1.2 into a runtime that agents can write to, resolvers can query, and ecosystems can integrate with.

A node is not a database. It is a governed artifact host. It runs the calculus, serves canonical artifacts, verifies capability chains, and derives adapter credentials. It does not define identity, own documents, or introduce semantics.

The protocol has exactly four endpoints:

| Endpoint | Role |
|---|---|
| `POST /resolve` | Run the SR-ABI calculus |
| `GET /sid/{digest}` | Serve an identity descriptor |
| `POST /verify-capability` | Verify a capability grant chain |
| `GET /adapter/{label}/{sid}` | Derive an ecosystem adapter credential |

---

## 0. Canonical Vocabulary

| Term | Meaning |
|---|---|
| node | A runtime host that implements DBC-NODE-1.0 |
| federation | A set of nodes sharing a common schema set and federation scope |
| schema set | The set of schemas a node recognizes and can resolve against |
| artifact store | The node's content-addressed store for canonical artifacts |
| descriptor store | The node's store for current and historical `IdentityDescriptor` objects |
| capability cache | The node's cache of verified capability grant chains |
| peer | Another node in the same or a bridged federation |
| request | A well-formed HTTP request conforming to the endpoint contract |
| response | A canonical JSON response conforming to the endpoint contract |

---

## Document Structure

| Section | Role |
|---|---|
| §1 — Node Model | What a node is and what it hosts |
| §2 — POST /resolve | SR-ABI calculus endpoint |
| §3 — GET /sid/{digest} | Descriptor resolution endpoint |
| §4 — POST /verify-capability | Capability verification endpoint |
| §5 — GET /adapter/{label}/{sid} | Adapter derivation endpoint |
| §6 — Federation Protocol | How nodes participate in a federation |
| §7 — Node Laws | The invariants a conforming node must maintain |
| §8 — Security | Threat model and mitigations |

---

## 1. Node Model

### 1.1 What a Node Is

A DBC node is a governed artifact host. It exposes four endpoints. It maintains an artifact store, a descriptor store, and a capability cache. It does not define identity, author schemas, or make semantic decisions on behalf of agents.

```
Agent
  ↓  SRCall
Node
  ↓  runs resolveTo(stage, input)
  ↓  stores NormalForm in artifact store
  ↓  projects IdentityDescriptor into descriptor store
  ↓  SRResult
Agent
```

The node's authority is exactly: run the calculus faithfully, serve what results.

### 1.2 What a Node Hosts

| Component | Contents | Authority |
|---|---|---|
| Artifact store | Canonical NormalForms, SRCalls, SRResults | Read-only after write |
| Descriptor store | IdentityDescriptors keyed by SID + epoch | Read-only after projection |
| Schema set | Admitted schemas the node can resolve against | Declared at node initialization |
| Capability cache | Verified capability grant chains | Derived; recomputed on expiry |

### 1.3 What a Node Does Not Do

A node must not:

- invent semantic content
- author capability grants on behalf of actors
- mutate canonical artifacts after storage
- serve a descriptor that disagrees with its artifact store
- accept schema changes at runtime without a new node initialization
- become a source of authority for any SID

### 1.4 Node Identity

A node itself has a SID, derived from its canonical descriptor:

```
node_sid = SID(
  schema_digest  = H(node_schema_set),
  normal_form    = normalize(node_descriptor),
  federation_scope = federation_name,
  derivation_context = { "role": "node" }
)
```

This SID is used to identify the node in federation peer lists and capability chains.

### 1.5 Transport

The protocol runs over HTTPS. All request and response bodies are `application/json`. All JSON objects must use canonical key order (lexicographic). Nodes must serve a valid TLS certificate. Nodes must not accept plaintext HTTP for production traffic.

---

## 2. POST /resolve

### 2.1 Purpose

Run the SR-ABI stage-indexed resolution calculus against a document and schema. Return a canonical `SRResult`.

This endpoint is the direct HTTP binding of `resolveTo(stage, input)` from DBC-1.2 §2.

### 2.2 Request

```
POST /resolve
Content-Type: application/json
```

Body: a canonical `SRCall` envelope as defined in DBC-1.2 §3.3.

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [...],
    "n": null,
    "sigma": { ... },
    "v": { ... }
  },
  "document_digest":  "sha256:<digest>",
  "null_digest":      "sha256:<digest>",
  "schema_digest":    "sha256:<digest>",
  "target_stage":     "Normalized",
  "view_digest":      "sha256:<digest>"
}
```

### 2.3 Response — Success

HTTP 200. Body: a canonical `SRResult` success envelope.

```json
{
  "canonical_value": { ... },
  "stage":           "Normalized",
  "value_digest":    "sha256:<digest>",
  "value_kind":      "NormalForm"
}
```

`value_kind` must match `target_stage` exactly per DBC-1.2 §3.4.

### 2.4 Response — Reject

HTTP 200. A reject is not an HTTP error. It is a canonical `SRResult` reject envelope.

```json
{
  "canonical_evidence": { ... },
  "reject_code":  "invalid_symbol",
  "reject_kind":  "RejectRealize",
  "stage":        "Realized"
}
```

Reject codes and kinds are defined in DBC-1.2 §3.5.

### 2.5 Response — Protocol Error

HTTP 400 if the request body is not a valid `SRCall`. HTTP 422 if the `schema_digest` is not in the node's schema set. HTTP 503 if the node is temporarily unable to run the calculus.

Protocol errors are not SR-ABI rejects. They indicate the request did not reach the calculus.

### 2.6 Storage Obligation

After a successful resolution at `target_stage = Normalized`, the node must store the `canonical_value` in its artifact store keyed by `value_digest`. The `SRCall` and `SRResult` pair must also be stored keyed by `document_digest + schema_digest + target_stage`.

If the resolution produces a `NormalForm`, the node must project an `IdentityDescriptor` and store it in the descriptor store, keyed by the derived SID and current epoch.

### 2.7 Idempotence

Identical `SRCall` requests must produce identical `SRResult` responses. The node must not introduce nondeterminism. If the artifact store already contains a result for this `document_digest + schema_digest + target_stage`, the node may return the cached result without recomputing.

### 2.8 Example

**Request:**

```json
{
  "abi_version": "SR-ABI",
  "canonical_input": {
    "d": [
      { "Rel": ["hasType", "alice", "Person"] }
    ],
    "n": null,
    "sigma": {
      "clause_formation": { "hasType": 2 },
      "closure_rules": [],
      "normalization_rules": [],
      "projection_constraints": [],
      "symbol_admission": ["Person", "alice"],
      "term_formation": {}
    },
    "v": {}
  },
  "document_digest":  "sha256:<digest>",
  "null_digest":      "sha256:<digest>",
  "schema_digest":    "sha256:<digest>",
  "target_stage":     "Realized",
  "view_digest":      "sha256:<digest>"
}
```

**Response:**

```json
{
  "canonical_value": {
    "constraints": [],
    "edges": [["hasType", "alice", "Person"]],
    "nodes": ["Person", "alice"],
    "witnesses": []
  },
  "stage":        "Realized",
  "value_digest": "sha256:<digest>",
  "value_kind":   "RealizedStructure"
}
```

---

## 3. GET /sid/{digest}

### 3.1 Purpose

Serve the current `IdentityDescriptor` for a SID. This is the registry replacement endpoint.

External systems use this to discover the public resolution surface of a semantic identity without querying a mutable registry.

### 3.2 Request

```
GET /sid/<sid_digest>
```

where `<sid_digest>` is the hex or base58 digest component of a `sid:dbc:` identifier.

Optional query parameters:

| Parameter | Meaning |
|---|---|
| `epoch` | Return the descriptor for a specific epoch. Defaults to current. |
| `format` | `native` (default) or `did` for DID document projection |

### 3.3 Response — Success

HTTP 200. Body: canonical `IdentityDescriptor` JSON as defined in DBC-IDL-1.2 §4.4.

```json
{
  "adapters": {
    "adapter:did:key": "did:key:z6Mk...",
    "adapter:evm":     "0x1a2b..."
  },
  "descriptor_digest": "sha256:<digest>",
  "epoch":             "<epoch>",
  "federation_scope":  "<federation>",
  "governors": [
    {
      "governor_sid":        "sid:dbc:<digest>",
      "verification_method": "did:key:z6Mk..."
    }
  ],
  "revocation": {
    "endpoint":          null,
    "revocation_digest": null
  },
  "schema_digest":  "sha256:<digest>",
  "services":       [],
  "sid":            "sid:dbc:<digest>",
  "spec":           "DBC-IDL-1.2"
}
```

### 3.4 Response — DID Projection

When `?format=did`, the node returns a W3C DID document derived from the descriptor per DBC-IDL-1.2 §4.8.

```json
{
  "@context": ["https://www.w3.org/ns/did/v1"],
  "assertionMethod":  ["did:dbc:<digest>#key-0"],
  "authentication":   ["did:dbc:<digest>#key-0"],
  "id":               "did:dbc:<digest>",
  "service":          [],
  "verificationMethod": [
    {
      "controller":         "did:dbc:<digest>",
      "id":                 "did:dbc:<digest>#key-0",
      "publicKeyMultibase": "z6Mk...",
      "type":               "Ed25519VerificationKey2020"
    }
  ]
}
```

The DID document is derived on demand from the current descriptor. It is never stored as a mutable registry entry.

### 3.5 Response — Not Found

HTTP 404 if the node has no descriptor for this SID. The node must not invent a descriptor.

### 3.6 Response — Stale Epoch

HTTP 410 Gone if the requested epoch is older than the current epoch and the node has purged it.

### 3.7 Verification Obligation

On serving a descriptor, the node must verify:

- `descriptor_digest` matches the document
- the descriptor's epoch is current
- the governor set is valid against the schema's root trust anchors

If any check fails, the node must return HTTP 500 with a diagnostic body rather than serve an invalid descriptor.

### 3.8 Federation Fallback

If the node has no descriptor for the requested SID, it should attempt to resolve it from federation peers before returning 404. If a peer returns a valid descriptor, the node may cache it and serve it.

---

## 4. POST /verify-capability

### 4.1 Purpose

Verify that a capability grant chain is valid: the grant is correctly signed by the governor, the governor chain traces to a root trust anchor, the epoch is current, and no revocation record applies.

### 4.2 Request

```
POST /verify-capability
Content-Type: application/json
```

Body:

```json
{
  "capability_grant": {
    "actor_sid":    "sid:dbc:<digest>",
    "constraints":  {},
    "epoch":        "<epoch>",
    "evidence":     {},
    "governor_sid": "sid:dbc:<digest>",
    "rights":       ["<right>"],
    "scope":        "<scope>",
    "subject_sid":  "sid:dbc:<digest>"
  },
  "governor_chain": [
    {
      "grant":     { ... },
      "signature": "<hex_signature>"
    }
  ],
  "signature": "<hex_signature_over_grant_by_governor>"
}
```

| Field | Meaning |
|---|---|
| `capability_grant` | The grant being verified |
| `signature` | Governor's signature over the canonical encoding of `capability_grant` |
| `governor_chain` | The chain of grants from root trust anchor to this governor |

### 4.3 Response — Valid

HTTP 200.

```json
{
  "epoch":      "<epoch>",
  "result":     "valid",
  "subject_sid": "sid:dbc:<digest>",
  "actor_sid":   "sid:dbc:<digest>",
  "scope":       "<scope>",
  "rights":      ["<right>"]
}
```

### 4.4 Response — Invalid

HTTP 200 with a non-valid result. A failed verification is not an HTTP error.

```json
{
  "reason":  "revoked",
  "result":  "revoked",
  "revocation_digest": "sha256:<digest>"
}
```

| Result value | Meaning |
|---|---|
| `valid` | Grant is current, correctly signed, and not revoked |
| `expired` | Grant epoch has passed |
| `revoked` | A revocation record exists for this grant |
| `invalid_signature` | Governor signature does not verify |
| `broken_chain` | Governor chain does not trace to a root trust anchor |
| `schema_mismatch` | Grant applies to a different schema than this node recognizes |

### 4.5 Verification Procedure

The node must perform these checks in order, failing fast:

1. Verify `capability_grant.epoch` is current
2. Verify `signature` over canonical encoding of `capability_grant` using `governor_sid`'s adapter credential
3. Verify `governor_chain` traces from `governor_sid` to a root trust anchor declared in the schema
4. Check revocation store for a `RevocationRecord` matching `H(capability_grant)`
5. All checks pass → return `valid`

### 4.6 Revocation Store

The node maintains a revocation store keyed by grant digest. Revocation records are themselves canonical artifacts governed by DBC-IDL-1.2 §2.5. The node must reject any revocation record not signed by a valid governor.

---

## 5. GET /adapter/{label}/{sid}

### 5.1 Purpose

Derive an ecosystem-specific adapter credential from a SID using the domain-separated HKDF derivation defined in DBC-IDL-1.2 §3.

This endpoint allows ecosystem tooling to derive credentials without implementing HKDF themselves.

### 5.2 Request

```
GET /adapter/<label>/<sid_string>
```

where:

- `<label>` is a URL-safe adapter label (e.g., `evm`, `did-key`, `bip39`, `git`, `ipfs`)
- `<sid_string>` is the full `sid:dbc:<digest>` string, URL-encoded

Examples:

```
GET /adapter/evm/sid%3Adbc%3A3f9a2b...
GET /adapter/did-key/sid%3Adbc%3A3f9a2b...
GET /adapter/bip39/sid%3Adbc%3A3f9a2b...
```

### 5.3 Response — Success

HTTP 200.

```json
{
  "adapter_label":    "adapter:evm",
  "credential":       "0x1a2b3c...",
  "credential_kind":  "evm_address",
  "sid":              "sid:dbc:<digest>"
}
```

| Field | Meaning |
|---|---|
| `adapter_label` | The full adapter label used for derivation (with `adapter:` prefix) |
| `credential` | The derived credential value |
| `credential_kind` | The type of credential returned |
| `sid` | The SID the credential was derived from |

`credential_kind` values per adapter:

| Adapter | `credential_kind` | `credential` form |
|---|---|---|
| `evm` | `evm_address` | `0x`-prefixed 20-byte hex |
| `did-key` | `did_key` | `did:key:z...` string |
| `bip39` | `bip39_mnemonic` | space-separated word list |
| `git` | `git_ref` | 40-char hex object ID |
| `ipfs` | `ipfs_cid` | CIDv1 string |

### 5.4 Response — Unknown Adapter

HTTP 404 if the adapter label is not in this node's adapter registry.

### 5.5 Public vs Private Credentials

Some adapter credentials are public by nature (EVM address, DID, Git ref, IPFS CID). Others are sensitive (BIP39 mnemonic, private keys). Nodes must clearly document which adapter endpoints they expose publicly and must require capability verification before serving sensitive credentials.

The `bip39` adapter must require a valid capability grant from the requesting actor demonstrating authority over the subject SID before returning the mnemonic. Public adapters (`evm`, `did-key`, `git`, `ipfs`) may be served without capability verification.

---

## 6. Federation Protocol

### 6.1 What Federation Means

A federation is a set of nodes that share a common schema set and federation scope. Because the calculus is deterministic:

```
same document + same schema → same NormalForm → same SID
```

nodes never need consensus about identity. They only need agreement on which schemas are admitted.

This is the core insight that enables registry-free federation: **identity emerges from shared calculus, not from shared state**.

### 6.2 Federation Topology

Nodes in a federation form a peer graph. Each node maintains a peer list:

```json
{
  "federation_scope": "<federation_name>",
  "peers": [
    {
      "endpoint":  "https://node-a.example.com",
      "node_sid":  "sid:dbc:<digest>",
      "epoch":     "<epoch>"
    }
  ],
  "schema_digests": ["sha256:<digest>", "sha256:<digest>"]
}
```

Nodes do not need to be fully connected. Descriptor resolution uses federation fallback (§3.8) to propagate discovery across the peer graph.

### 6.3 Schema Consensus

Before federation, nodes must agree on the schema set. Schema agreement means:

- all nodes in the federation recognize the same `schema_digest` values
- all nodes run identical resolution logic against those schemas
- nodes that do not recognize a `schema_digest` must return HTTP 422 for calls against that schema

Schema addition requires an out-of-band governance decision and a new federation initialization for nodes that accept the new schema. Schemas are never added at runtime.

### 6.4 Descriptor Propagation

When a node stores a new `IdentityDescriptor`, it should announce the SID and epoch to its peers. Announcement is a best-effort notification:

```
POST /federation/announce
```

Body:

```json
{
  "descriptor_digest": "sha256:<digest>",
  "epoch":             "<epoch>",
  "node_sid":          "sid:dbc:<digest>",
  "sid":               "sid:dbc:<digest>"
}
```

Peers that receive an announcement may fetch the descriptor via `GET /sid/{digest}` and cache it locally. Announcements are advisory. Nodes must verify descriptors independently.

### 6.5 Epoch Coordination

Epochs provide a time boundary for capability grants and descriptor validity. Within a federation, nodes should use a shared epoch definition (e.g., a consensus round, a time window, or a governance block). Nodes operating on different epochs must not serve each other's grants or descriptors as current.

### 6.6 Convergence Property

Because the calculus is deterministic and normalization is confluent (DBC-1.2 §2.7), a federation of honest nodes will always converge on the same `NormalForm` and therefore the same `SID` for any given document and schema. This is the building correspondence: the federation is a distributed realization of the same canonical apartment.

Divergence is only possible if:

- nodes run different schemas (schema mismatch → HTTP 422)
- a node introduces nondeterminism (conformance violation)
- a document is genuinely different (different input → different SID, which is correct)

---

## 7. Node Laws

A conforming node must maintain all of the following invariants.

### 7.1 Calculus Faithfulness

```
The node runs resolveTo(stage, input) exactly as defined in DBC-1.2.
Same SRCall → same SRResult.
```

The node must not introduce additional rewrite rules, modified schemas, or nondeterministic behavior.

### 7.2 Artifact Immutability

```
Once a canonical artifact is stored, it must never be modified.
```

New epochs produce new descriptors. They do not modify prior artifacts. The artifact store is append-only.

### 7.3 Descriptor Non-Authority

```
The node's descriptor store describes governed state.
It does not define it.
```

If governed state changes (new governor, new capability grant, new adapter binding), the node must re-project a new descriptor. It must not directly edit the descriptor.

### 7.4 No Semantic Invention

```
The node must not introduce semantic content.
```

The node runs the calculus on submitted documents. It does not author documents, create schemas, or infer meaning beyond what the calculus produces.

### 7.5 Schema Closure

```
The node's schema set is fixed at initialization.
```

Runtime schema addition is not permitted. A node that encounters an unknown `schema_digest` must return HTTP 422.

### 7.6 Capability Verification Completeness

```
The node must check all five verification steps (§4.5) in order.
A partial verification is not conforming.
```

### 7.7 Adapter Isolation

```
The node's adapter derivations are domain-separated per DBC-IDL-1.2 §3.2.
A compromise in one adapter domain must not affect others.
```

### 7.8 Sensitive Credential Protection

```
Sensitive adapter credentials (BIP39 mnemonic, private keys) must not be
served without a valid, verified capability grant from the requesting actor.
```

---

## 8. Security

### 8.1 Threat Model

The node protocol assumes:

- network adversaries can observe and replay requests
- storage adversaries may attempt to inject false artifacts
- federation peers may be honest-but-stale or actively malicious
- ecosystem adapters may be targeted for credential extraction

### 8.2 Replay Protection for /resolve

Requests to `/resolve` are idempotent by design. Replay attacks produce the same result and are harmless. Nodes need not implement replay protection for this endpoint beyond standard TLS.

### 8.3 Descriptor Integrity

Every `IdentityDescriptor` includes a `descriptor_digest`. Nodes must verify this digest on receipt from peers. A descriptor that fails digest verification must be discarded and must not be served to clients.

### 8.4 Capability Chain Verification

The governor chain must be verified to the root trust anchor before any capability grant is accepted as valid. An unverifiable chain must return `broken_chain`, not `valid`.

### 8.5 Sensitive Adapter Credential Extraction

The `/adapter/bip39` and any private-key-producing adapter endpoints must be protected by capability grant verification. Nodes should log all accesses to these endpoints.

### 8.6 Federation Peer Trust

Nodes must not blindly trust artifacts from federation peers. Every artifact received from a peer must be independently verified:

- descriptors: verify `descriptor_digest`
- capability grants: run full §4.5 verification
- SRResults: verify `value_digest` against recomputed canonical value if the node ran the calculus itself

Peers are trusted for routing and availability, not for semantic authority.

### 8.7 Schema Pinning

The node's schema set is fixed at initialization and content-addressed by digest. A schema change attack (substituting a different schema under the same digest) is defeated by collision resistance of the hash function.

---

## 9. Summary

### 9.1 The Four Endpoints

| Endpoint | Method | Role | Returns |
|---|---|---|---|
| `/resolve` | POST | Run the SR-ABI calculus | `SRResult` |
| `/sid/{digest}` | GET | Serve identity descriptor | `IdentityDescriptor` or DID document |
| `/verify-capability` | POST | Verify capability grant chain | `valid` / `expired` / `revoked` / etc. |
| `/adapter/{label}/{sid}` | GET | Derive adapter credential | Ecosystem-specific credential |

### 9.2 The Node Invariants

```
calculus faithfulness
artifact immutability
descriptor non-authority
no semantic invention
schema closure
capability verification completeness
adapter isolation
sensitive credential protection
```

### 9.3 The Full Stack

```
Agent
  ↓  SRCall
  ↓
POST /resolve          [DBC-1.2 calculus]
  ↓
NormalForm → artifact store
NormalForm → IdentityDescriptor → descriptor store
  ↓
GET /sid/{digest}      [DBC-IDL-1.2 descriptor]
  ↓
POST /verify-capability  [DBC-IDL-1.2 capability chain]
  ↓
GET /adapter/{label}/{sid}  [DBC-IDL-1.2 adapter derivation]
  ↓
Ecosystem
```

### 9.4 The Federation Property

```
same document + same schema
  → same NormalForm       (DBC-1.2 confluence)
  → same SID              (DBC-IDL-1.2 SID determinism)
  → same descriptor       (DBC-IDL-1.2 descriptor determinism)
  → no registry required
```

Federation is possible because **identity emerges from shared calculus, not from shared state**.

---

## 10. Relation to DBC-1.2 and DBC-IDL-1.2

| Concept | Defined in | Node role |
|---|---|---|
| `SRCall` / `SRResult` envelopes | DBC-1.2 §3 | Accepted / returned by `/resolve` |
| `resolveTo(stage, input)` | DBC-1.2 §2 | Executed by `/resolve` |
| `NormalForm` | DBC-1.2 §2.6 | Stored by node after resolution |
| `SID` derivation | DBC-IDL-1.2 §1 | Computed from stored NormalForms |
| `IdentityDescriptor` | DBC-IDL-1.2 §4 | Projected and served by `/sid/{digest}` |
| `CapabilityGrant` verification | DBC-IDL-1.2 §2 | Executed by `/verify-capability` |
| Adapter derivation | DBC-IDL-1.2 §3 | Executed by `/adapter/{label}/{sid}` |
| DID projection | DBC-IDL-1.2 §4.8 | Served by `/sid/{digest}?format=did` |

The node protocol does not extend or modify DBC-1.2 or DBC-IDL-1.2. It binds their definitions to HTTP endpoints.

---

## Changelog

| Version | Change |
|---|---|
| DBC-NODE-1.0 | Initial specification |

---

*DBC-NODE-1.0 · DBC Blackboard Node Protocol · Brian Thorne · bthornemail*
