# DBC Identity Layer

**DBC-IDL-1.2 · Specification**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
Status: Draft Specification · Audience: maintainers, implementers, ecosystem adapters  
Depends on: DBC-1.2

---

## Abstract

The DBC Identity Layer (IDL) defines how canonical semantic identity is derived from normalized blackboard artifacts, how authority over that identity is delegated through capability grants, how ecosystem-specific credentials are produced through domain-separated adapters, how public resolution state is described through a canonical identity descriptor, and how identity is presented to human and machine observers through projection.

The layer is organized around one architectural principle:

> **Identity is canonical. Authority is delegated. Credentials are adapters. Descriptors are discoverable. Presentation is projection.**

This inverts the assumption of all major existing identity systems. Identity is not derived from keys. Keys are derived from identity — if they are needed at all. Discovery does not require a mutable registry. It requires a canonical projection of governed state.

---

## 0. Canonical Vocabulary

| Term | Meaning |
|---|---|
| SID | Semantic Identity — the canonical, content-derived identifier of an entity |
| FIDX | Federation Index — a scoped SID within a named federation domain |
| capability grant | A signed assertion that an actor may act for a subject under defined scope and constraints |
| adapter | A domain-separated derivation that produces an ecosystem-specific credential from a SID |
| adapter credential | The output of an adapter: an EVM address, BIP39 seed, DID document, device key, etc. |
| identity descriptor | A canonical, read-only, resolvable artifact that describes the public resolution surface of a SID |
| identity projection | A human-readable or protocol-readable presentation of a SID, descriptor, or credential |
| governor | The SID that has authority to issue capability grants for a given subject |
| actor | The SID exercising a capability |
| subject | The SID for which a capability is granted |
| epoch | A bounded time window or consensus round within which a capability is valid |
| derivation context | Additional input to SID derivation that scopes meaning without changing schema |
| resolution surface | The set of public verification methods, service endpoints, and adapter bindings published for a SID |

---

## Document Structure

| Layer | Role |
|---|---|
| Layer 1 — Semantic Identity | How SID is derived from canonical normal form |
| Layer 2 — Capability Authority | How authority over a SID is delegated and constrained |
| Layer 3 — Adapter Derivation | How ecosystem-specific credentials are produced from a SID |
| Layer 4 — Identity Descriptor | How a SID's public resolution surface is described without a registry |
| Layer 5 — Identity Projection | How SID, descriptors, and credentials are presented to observers |

---

## 1. Semantic Identity

### 1.1 Position in the Stack

Semantic identity sits above the DBC resolution calculus. It is computed from the output of `resolveTo(Normalized, i)` — the canonical normal form produced by the blackboard.

```
document
  → realize → close → normalize
  → NormalForm N
  → SID(N)                         [identity layer begins here]
  → capability binding
  → adapter derivation
  → identity projection
```

The blackboard produces truth. The identity layer derives identity from that truth. The two layers must not be collapsed.

### 1.2 SID Definition

```
SID = H(
  schema_digest,
  canonical_normal_form,
  federation_scope,
  derivation_context
)
```

where `H` is a collision-resistant hash function (e.g., BLAKE3 or SHA-256) applied to the canonical byte encoding of the tuple.

| Component | Role |
|---|---|
| `schema_digest` | Pins the admissibility law under which the normal form was produced |
| `canonical_normal_form` | The unique canonical representative of the entity's meaning |
| `federation_scope` | The named domain within which this identity is scoped |
| `derivation_context` | Additional scoping that does not change schema or meaning |

### 1.3 SID Laws

**Determinism**

```
same inputs → same SID
```

The SID is purely a function of its inputs. No randomness, clock time, or external state may influence it.

**Canonical dependency**

```
SID(N) is valid only if N is a NormalForm
```

A SID may not be derived from a `RealizedStructure` or `ClosedStructure`. Only normalized canonical artifacts produce stable identity.

**Schema pinning**

```
SID(N, σ1) ≠ SID(N, σ2)  when  σ1 ≠ σ2
```

Two entities that are identical in structure but were admitted under different schemas are distinct identities. Schema is part of meaning.

**Meaning primacy**

```
identity is structural, not credential
```

A SID does not depend on any key, wallet, token, or provider. Keys are downstream adapters, not upstream sources.

### 1.4 FIDX — Federation-Scoped Identity

When `federation_scope` is named, the SID becomes a FIDX (Federation Index):

```
FIDX = SID with federation_scope ≠ null
```

A FIDX is a SID that is scoped to a specific federation domain. The same entity may have different FIDXes in different federation domains, all derivable from the same underlying normal form with different `federation_scope` inputs.

This enables cross-domain portability: the entity's meaning is shared, but its identity is scoped.

### 1.5 What SID Answers

```
SID answers:  what entity is this?
```

It does not answer:

- who controls this entity
- who may act for this entity
- how this entity signs transactions
- where this entity is stored

Those are answered by the capability layer and adapter layer.

### 1.6 Entities That May Have SIDs

A SID may be derived for any entity whose canonical meaning can be expressed as a normalized blackboard artifact:

| Entity type | Example |
|---|---|
| Person | `hasType(alice, Person)` normalized |
| Organization | normalized org structure |
| Software module | normalized function/module signature |
| Document | normalized document structure |
| Device | normalized hardware descriptor |
| Service | normalized service specification |
| World / space | normalized scene graph |
| Agent | normalized agent descriptor |
| Schema | the schema itself, normalized |

Anything with a canonical normal form has a canonical identity.

### 1.7 Canonical SID Encoding

The SID hash input must be a deterministically encoded byte string. Different implementations must produce identical SIDs for the same logical inputs.

The canonical encoding is a tagged length-value (TLV) concatenation in fixed field order:

```
SID_input_bytes =
  tag("schema_digest")        || encode(schema_digest)        ||
  tag("normal_form")          || encode(canonical_normal_form) ||
  tag("federation_scope")     || encode(federation_scope)      ||
  tag("derivation_context")   || encode(derivation_context)
```

where:

- `tag(name)` is the UTF-8 bytes of the field name prefixed by a 2-byte big-endian length
- `encode(value)` is the canonical JSON byte encoding of the value (UTF-8, no trailing whitespace, keys in canonical order) prefixed by a 4-byte big-endian length
- `federation_scope` encodes as the empty string `""` when null
- `derivation_context` encodes as the empty object `{}` when absent

The hash domain tag is prepended before hashing:

```
SID = H( "DBC-SID-1.1" || 0x00 || SID_input_bytes )
```

The domain tag `"DBC-SID-1.1"` followed by a null byte separates SID computation from any other use of the same hash function in the stack. Implementations must not omit the domain tag.

**Encoding law:**

```
same logical SID inputs → same SID_input_bytes → same SID
```

Any implementation that deviates from this encoding produces a non-conforming SID and must be rejected at interop boundaries.

### 1.8 SID Prefix Namespace

SIDs must be represented in text using a prefix namespace that identifies the spec version and, optionally, the federation scope.

**Unscoped SID:**

```
sid:dbc:<base58_or_hex_digest>
```

**Federation-scoped SID (FIDX):**

```
fidx:<federation_name>:<base58_or_hex_digest>
```

Examples:

```
sid:dbc:3f9a2b...
fidx:metaverse-kit:7c14e0...
```

The prefix namespace serves three purposes: it prevents digest collisions with other hash uses, it makes SIDs recognizable to tooling without out-of-band context, and it allows federation scope to be read from the identifier string without resolving the full artifact.

**Namespace law:**

```
A bare digest with no prefix is not a valid SID in text form.
```

## 2. Capability Authority

### 2.1 The Split

Identity and authority are separate.

```
SID:         what an entity is
Capability:  who may act for it, under what constraints
```

This split is the architectural core of the identity layer. Collapsing it — making the identity directly controlled by a key — reduces the system to conventional key-first identity and loses all structural advantages.

### 2.2 Capability Grant Definition

A capability grant is a signed assertion that an actor may perform operations within a defined scope on behalf of a subject. The grant is **signed by the governor**, not by the actor.

```
CapabilityGrant = Sign_governor(
  subject_sid,
  actor_sid,
  governor_sid,
  scope,
  rights,
  epoch,
  constraints,
  evidence
)
```

| Field | Meaning |
|---|---|
| `subject_sid` | The SID for which authority is being granted |
| `actor_sid` | The SID receiving the grant |
| `governor_sid` | The SID issuing and signing this grant |
| `scope` | The operation class or domain to which the grant applies |
| `rights` | The specific rights being granted within that scope |
| `epoch` | The time window or consensus round in which the grant is valid |
| `constraints` | Additional restrictions on exercise of the grant |
| `evidence` | Attestation or proof that the governor has authority to issue |

The governor signs using its adapter credential (e.g., its EVM key or DID key). The actor does not sign the grant; the actor **presents** it. The subject's SID is structural and does not itself sign.

This is the functional correctness requirement: if the actor signed its own grant, the No Self-Authorization law would be unenforceable. The governor signature makes the chain of authority verifiable independently of the actor.

### 2.3 Capability Laws

**Delegation law**

```
Actor A may act for Subject S
only if A holds a valid CapabilityGrant from a Governor G
that is itself authorized over S.
```

**No self-authorization**

```
A SID does not authorize itself.
```

Authority always comes from a grant. There is no implicit root authority derived from holding a SID.

**Governor chain**

```
Governor authority is itself a CapabilityGrant
or a root trust anchor declared in the schema.
```

The schema may declare root governors. All other authority chains from there.

**Epoch-boundedness**

```
A CapabilityGrant is valid only within its declared epoch.
Expired grants must be rejected.
```

**Non-confusion law**

```
A CapabilityGrant over SID(N, σ1) does not apply to SID(N, σ2).
```

Grants are SID-specific. Schema change changes the SID, which invalidates all prior grants.

### 2.4 Delegation Chain

Delegation is first-class. A governor may delegate authority further:

```
SID(subject)
  ← CapabilityGrant from SID(root_governor)
  → delegated to SID(sub_governor)
  → further delegated to SID(actor)
```

Each link in the chain is a CapabilityGrant. The chain must be verifiable from root to actor.

### 2.5 Revocation

Capability grants may be revoked by the issuing governor within the same epoch. Revocation is itself a signed artifact:

```
RevocationRecord = Sign_governor(
  grant_digest,
  revocation_reason,
  epoch
)
```

Implementations must check for revocation records before accepting a grant.

### 2.6 Why This Is Better Than Key-First Authority

Key-first authority says:

```
I hold the private key → I am the identity → I may act
```

This conflates three distinct things: possession, identity, and authority.

Capability-first authority says:

```
This entity has a canonical identity (SID).
This actor holds a grant to act for it.
The grant defines what they may do.
```

This allows:

- multiple actors to hold different capability grants for the same subject
- capability grants to be time-bounded and revocable without rotating the identity
- groups, services, and devices to have stable identity independent of key rotation
- the identity to survive key compromise as long as governance is intact

---

## 3. Adapter Derivation

### 3.1 Purpose

Adapters produce ecosystem-specific credentials from a SID using domain-separated key derivation. They allow the same semantic entity to participate in multiple external ecosystems without conflating those ecosystems or coupling them to the semantic layer.

### 3.2 Adapter Law

```
adapter(SID, domain) → ecosystem_credential
```

Every adapter must use domain-separated derivation:

```
seed = HKDF(SID, domain_label)
```

where `domain_label` is a unique, human-readable string identifying the adapter and ecosystem (e.g., `"adapter:evm"`, `"adapter:bip39"`, `"adapter:did:key"`).

**Isolation law**

```
adapter(SID, "adapter:evm") and adapter(SID, "adapter:bip39")
are cryptographically independent.
```

A compromise of one adapter domain does not compromise others.

**Non-reversal law**

```
ecosystem_credential ↛ SID
```

It must be computationally infeasible to recover the SID from an adapter credential. The semantic layer is always upstream.

**Determinism law**

```
same SID + same domain_label → same ecosystem_credential
```

Adapters are pure functions. No randomness is introduced at the adapter layer.

### 3.3 EVM Adapter

```
adapter(SID, "adapter:evm"):
  seed    = HKDF(SID, "adapter:evm")
  privkey = secp256k1_scalar(seed)
  pubkey  = secp256k1_pubkey(privkey)
  address = keccak256(pubkey)[12:]
```

Result: a deterministic Ethereum address bound to the semantic identity.

The same semantic structure always maps to the same address. The EVM adapter is one projection of identity, not the source of it.

### 3.4 BIP39 / HD Wallet Adapter

```
adapter(SID, "adapter:bip39"):
  seed     = HKDF(SID, "adapter:bip39")
  entropy  = seed[0:16]   -- or 0:32 for 24-word
  mnemonic = BIP39Encode(entropy)
  bip32    = BIP32Root(mnemonic)
```

Result: a deterministic mnemonic and BIP32 wallet root.

The wallet is a structural consequence of the entity's canonical identity. Recovery of the wallet requires knowledge of the SID inputs, not a separately stored seed phrase.

### 3.5 DID Adapter

```
adapter(SID, "adapter:did:key"):
  seed    = HKDF(SID, "adapter:did:key")
  keypair = Ed25519(seed)
  did     = "did:key:" + multibase(keypair.public)
```

Result: a deterministic `did:key` identifier.

The DID document becomes a projection of the semantic identity. Resolution of the DID produces the DID document; the DID document describes capabilities, not meaning.

### 3.6 Git Object Adapter

```
adapter(SID, "adapter:git"):
  object_id = SID[0:20]   -- or full SID as custom ref
```

Result: a deterministic Git object reference.

This allows semantic entities to have stable Git identities that survive content mutation, as long as their normalized meaning does not change.

### 3.7 Device Key Adapter

```
adapter(SID, "adapter:device:<device_class>"):
  seed       = HKDF(SID, "adapter:device:" + device_class)
  device_key = device_class_keygen(seed)
```

Result: a device-class-specific key bound to the semantic identity of the device.

### 3.8 IPFS / CID Adapter

```
adapter(SID, "adapter:ipfs"):
  cid = CIDv1(sha256, SID_bytes)
```

Result: a deterministic content identifier that can anchor the semantic entity in a content-addressed store.

### 3.9 Adapter Registry

Adapters must be declared in a registry. An undeclared adapter must not be executed.

| Adapter label | Ecosystem | Output |
|---|---|---|
| `adapter:evm` | Ethereum / EVM chains | 20-byte address |
| `adapter:bip39` | HD wallets | mnemonic + BIP32 root |
| `adapter:did:key` | W3C DID | `did:key` identifier |
| `adapter:git` | Git | object ref |
| `adapter:ipfs` | IPFS | CIDv1 |
| `adapter:device:<class>` | Hardware | device key |
| `adapter:oauth:<provider>` | OAuth | provider-specific principal |

Custom adapters may be added by declaring a new domain-separated label. They must not reuse existing labels.

---

## 4. Identity Descriptor

### 4.1 Purpose and Position

The `IdentityDescriptor` is a canonical, read-only, resolvable artifact that describes the public resolution surface of a SID. It is the single governed artifact that allows external systems to discover how to interact with a semantic identity — without requiring a mutable registry to define that identity.

```
SID
  → CapabilityGrant(s)       [govern who may act]
  → AdapterCredential(s)     [define ecosystem forms]
  → IdentityDescriptor       [publish resolution surface]
  → IdentityProjection       [present to observers]
```

The descriptor sits between the adapter layer and the projection layer. It is derived from SID-governed artifacts. It does not define them.

### 4.2 Why This Replaces DID Registries

Traditional DID registries are needed because the DID itself carries insufficient information for resolution. External systems must query a registry to find a mutable document that describes the identity.

```
DID → registry lookup → mutable DID document → resolution
```

The DBC-IDL architecture does not require this because:

- identity is already canonical (SID)
- governance is already artifact-based (CapabilityGrant)
- adapters are already deterministic (AdapterCredential)
- projection is already read-only

So the descriptor is not a source of truth. It is a **projection of truth**:

```
SID → canonical descriptor projection → resolution
```

The document is not authoritative over any layer above it. If the descriptor and the governed state disagree, the governed state is correct and the descriptor is stale.

### 4.3 IdentityDescriptor Definition

An `IdentityDescriptor` is the canonical projection of the current public state of a SID:

```
IdentityDescriptor(SID) =
  canonical projection of:
    SID
    + active governance state (governor set or governor reference)
    + active adapter bindings
    + public verification methods
    + revocation reference(s)
    + service endpoint(s)
    + federation scope
    + schema pin
```

### 4.4 Canonical Form

The native canonical form of an `IdentityDescriptor` is a JSON object with the following structure and canonical key order:

```json
{
  "adapters": {
    "adapter:did:key": "did:key:z6Mk...",
    "adapter:evm":     "0x1a2b..."
  },
  "descriptor_digest": "sha256:<digest_of_this_document>",
  "epoch":             "<epoch_identifier>",
  "federation_scope":  "<federation_name_or_null>",
  "governors": [
    {
      "governor_sid": "sid:dbc:<digest>",
      "verification_method": "did:key:z6Mk..."
    }
  ],
  "revocation": {
    "endpoint":          "<optional_revocation_endpoint>",
    "revocation_digest": "sha256:<latest_revocation_record_digest_or_null>"
  },
  "schema_digest":  "sha256:<schema_digest>",
  "services": [
    {
      "id":           "sid:dbc:<sid>#service-0",
      "serviceEndpoint": "https://...",
      "type":         "<service_type>"
    }
  ],
  "sid":     "sid:dbc:<digest>",
  "spec":    "DBC-IDL-1.2"
}
```

All fields must be present. Fields with no applicable value use `null` for scalars and `[]` for arrays. Keys are in canonical lexicographic order.

### 4.5 Field Definitions

| Field | Meaning |
|---|---|
| `sid` | The SID this descriptor describes |
| `spec` | The IDL spec version that governs this descriptor's format |
| `schema_digest` | The schema digest component of the SID |
| `federation_scope` | The federation scope component of the SID, or `null` |
| `epoch` | The epoch in which this descriptor is valid |
| `governors` | The current set of governors and their public verification methods |
| `adapters` | The active adapter credentials keyed by adapter label |
| `revocation` | Revocation reference: latest revocation digest and optional endpoint |
| `services` | Published service endpoints bound to this SID |
| `descriptor_digest` | SHA-256 digest of this descriptor document at canonical encoding |

### 4.6 Descriptor Laws

**Non-authority law**

```
IdentityDescriptor ↛ SID
IdentityDescriptor ↛ CapabilityGrant
IdentityDescriptor ↛ AdapterCredential
```

The descriptor is never authoritative over the layers above it. It describes; it does not define.

**Read-only law**

The descriptor is produced by projection from governed state. It is never updated by direct write. When governed state changes (new governor, new adapter binding, new service endpoint), a new descriptor is produced by re-projection.

**Canonical determinism**

```
same governed state → same descriptor
```

Two honest implementations projecting the same governed state at the same epoch must produce byte-identical descriptors.

**Stale descriptor rule**

If a descriptor is stale (governed state has changed since it was produced), external systems must re-resolve. A stale descriptor must not be used to override current governed state.

**Epoch validity**

A descriptor is valid only within the epoch in which it was produced. Descriptors from prior epochs must not be accepted as current.

### 4.7 Resolution Without a Registry

Given a SID, resolution proceeds as follows:

1. Derive the SID from canonical inputs (or accept a presented `sid:dbc:` string)
2. Locate the current `IdentityDescriptor` for that SID via a content-addressed store, federation index, or peer announcement
3. Verify the `descriptor_digest` matches the document
4. Verify the `epoch` is current
5. Verify the governor set against the schema's root trust anchors
6. Use the `governors`, `adapters`, `revocation`, and `services` fields for interaction

Step 2 does not require a central registry. It requires a content-addressed lookup. The descriptor is findable by its `sid` and `epoch` without a mutable index because the descriptor content is itself canonical and hashable.

### 4.8 DID-Compatible Projection

The `IdentityDescriptor` can be projected to a W3C DID document for ecosystem compatibility:

```
DID form:
  did:dbc:<sid_digest>

DID document projection from IdentityDescriptor:
{
  "@context": ["https://www.w3.org/ns/did/v1"],
  "id":                   "did:dbc:<sid_digest>",
  "verificationMethod":   [<derived from governors>],
  "authentication":       [<derived from governors with auth rights>],
  "assertionMethod":      [<derived from capability grants>],
  "service":              [<derived from services>]
}
```

This projection is exactly that — a projection. The DID document does not exist as a mutable registry entry. It is re-derived on demand from the canonical `IdentityDescriptor`. Any DID resolver for the `did:dbc` method must implement this derivation.

The DID document projection obeys the same no-backflow rule:

```
DID document projection ↛ IdentityDescriptor
DID document projection ↛ SID
```

### 4.9 Comparison with DID Registries

| Property | DID registry | DBC-IDL IdentityDescriptor |
|---|---|---|
| Source of truth | Mutable registry document | Canonical governed state |
| Update mechanism | Registry write operation | Re-projection from governed state |
| Identity definition | Registry document defines identity | SID defines identity; descriptor describes it |
| Availability | Depends on registry liveness | Content-addressable, replicable |
| Revocation | Registry update | Governed revocation artifact |
| Key rotation | Registry document update | New adapter credential; SID unchanged |
| Ecosystem compatibility | Native DID | DID projection available |

The fundamental difference: in a DID registry, the document is authoritative. In DBC-IDL, the governed state is authoritative and the descriptor is its projection.

---

## 5. Identity Projection

### 4.1 Position

Identity projection is the final layer. It is governed by the same no-backflow rule as DBC surface projection.

```
SID
  → adapter credential (or directly)
  → identity projection
  → human-readable display / protocol-readable document
```

Projection is observational. It does not modify SID, capability grants, or adapter credentials.

### 4.2 Projection Types

| Projection type | Output |
|---|---|
| display label | human-readable name or alias |
| wallet address display | formatted ecosystem address |
| DID URL | `did:...` string |
| QR code / deep link | scannable credential reference |
| capability summary | human-readable rights description |
| identity document | JSON-LD or equivalent structured form |

### 4.3 Projection Law

```
identity_projection(SID) is a view of SID, not a redefinition of it.
```

A display name is not an identity. A wallet address display is not the source of identity. These are presentations of an underlying structural fact.

### 4.4 No-Backflow Rule

```
identity_projection ↛ SID
identity_projection ↛ CapabilityGrant
identity_projection ↛ AdapterCredential
identity_projection ↛ IdentityDescriptor
```

Projection outputs must not be used as inputs to the identity, capability, adapter, or descriptor layers. This mirrors the DBC no-backflow rule for surface projection.

---

## 6. The Five-Object Model

The identity layer defines exactly five object types.

| Object | Layer | Derives from | Answers |
|---|---|---|---|
| `SemanticIdentity` (SID) | Identity | `NormalForm` | What is this entity? |
| `CapabilityGrant` | Authority | SID + governor | Who may act for it? |
| `AdapterCredential` | Adapter | SID + domain label | How does it appear in ecosystem X? |
| `IdentityDescriptor` | Descriptor | SID + governed state | How is it resolved and discovered? |
| `IdentityProjection` | Projection | SID, descriptor, or credential | How is it presented? |

These five objects form a strict partial order:

```
SemanticIdentity (SID)
       ↓
CapabilityGrant  ←── Sign_governor  (governor's AdapterCredential signs)
       ↓
AdapterCredential
       ↓
IdentityDescriptor   [projects governed state; non-authoritative]
       ↓
IdentityProjection   [presents to observers; non-authoritative]
```

The governor's `AdapterCredential` is used to sign the `CapabilityGrant`, but the credential is downstream of the SID and does not influence it. The signing relationship is a one-way production.

`IdentityDescriptor` and `IdentityProjection` are both downstream of governed state. Neither may influence any layer above them. No object at a lower layer may influence an object at a higher layer.

---

## 7. Cross-Ecosystem Coherence

### 6.1 One Entity, Many Ecosystems

A single semantic entity can participate coherently in multiple ecosystems:

```
NormalForm N
  → SID
  → adapter:evm          → Ethereum address
  → adapter:bip39        → HD wallet
  → adapter:did:key      → DID
  → adapter:git          → Git ref
  → adapter:ipfs         → CID
  → adapter:device:led   → device key
```

All of these are the same entity. None of them defines it. The definition is always the normalized canonical form.

### 6.2 Cross-Ecosystem Attestation

Because all adapters derive from the same SID, an entity can attest its cross-ecosystem coherence:

```
Sign_evm_key(SID)  proves EVM address is bound to SID
Sign_did_key(SID)  proves DID is bound to SID
```

Any observer who knows the SID can verify that an EVM address and a DID represent the same underlying semantic entity, without a central registry.

### 6.3 Ecosystem Independence

Compromise of one adapter does not compromise others:

```
compromise of adapter:evm credentials
  does not affect adapter:did:key credentials
  does not affect SID
  does not affect CapabilityGrants (those require governor authority)
```

Isolation is guaranteed by the domain-separation law.

---

## 8. Comparison with Existing Systems

### 7.1 Wallets

| Property | Wallet (key-first) | DBC-IDL |
|---|---|---|
| Identity source | Random private key | Canonical normal form |
| Identity stability | Lost if key lost | Stable as long as meaning is stable |
| Cross-ecosystem | Manual coordination | Structural via adapter derivation |
| Authority model | Key possession | Capability grant |

### 7.2 OAuth

| Property | OAuth | DBC-IDL |
|---|---|---|
| Identity source | Provider account | Canonical normal form |
| Portability | Provider-dependent | Structural |
| Authority model | Token-based | Capability grant |
| Revocation | Provider-controlled | Governor-controlled |

### 7.3 DID

| Property | DID | DBC-IDL |
|---|---|---|
| Identity source | Key or method-specific | Canonical normal form |
| Verifiability | DID document + resolution | Schema-pinned SID |
| Authority model | Verification method | Capability grant |
| Semantic binding | None (identifier only) | Full (identity = meaning) |

### 7.4 Git Object IDs

| Property | Git SHA | DBC-IDL |
|---|---|---|
| Identity source | Content hash | Semantic normal form |
| Semantic stability | Content-dependent | Meaning-dependent |
| Cross-ecosystem | Not supported | Via adapter derivation |

In every case, the DBC-IDL model is strictly more expressive because identity is derived from meaning, not from a system-specific artifact.

---

## 9. Security Properties

### 9.1 SID collision resistance

The SID is collision-resistant if `H` is collision-resistant. Two distinct entities cannot share a SID unless `H` is broken.

### 9.2 Adapter isolation

Domain-separated HKDF ensures that adapter credentials in different domains are computationally independent. No cross-domain inference is possible from a single adapter credential.

### 9.3 Capability forgery resistance

A capability grant is signed by the governor's adapter credential. Forgery requires compromise of that credential. The grant also chains to a governor; a forged grant with an invalid governor chain must be rejected.

### 9.4 Semantic stability under key rotation

Because the SID is not derived from any key, rotating an adapter credential (EVM private key, DID key, device key) does not change the SID. Identity is stable under key rotation.

### 9.5 Meaning-change detection

If the underlying entity changes its canonical meaning, the normal form changes, and therefore the SID changes. This is a feature: a different meaning is a different entity.

---

## 10. The Doctrine

The shortest complete statement of the DBC identity layer:

```
Canonical semantic identity determines what an entity is.
Capabilities determine who may act for it.
Adapters determine how that authority appears in external ecosystems.
Descriptors expose its public resolution surface without a registry.
Projections determine how it is displayed.
```

This doctrine applies at every layer of the stack without exception.

---

## 11. Relation to DBC-1.2

The identity layer is a governed artifact layer above the resolution calculus.

| DBC-1.2 concept | IDL concept |
|---|---|
| `NormalForm` | Source of `SemanticIdentity` |
| `resolveTo(Normalized, i)` | Produces the canonical input to SID derivation |
| Schema digest | Component of SID computation |
| `project` operator | Parallel to `IdentityDescriptor` projection |
| `emit` / `SurfaceArtifact` | Parallel to `IdentityProjection` |
| No-backflow rule | Governs `IdentityDescriptor ↛ SID` and `IdentityProjection ↛ SID` |
| Confluence law | Guarantees SID uniqueness for same input |
| Schema preservation | Guarantees SID is schema-bound |

The identity layer does not modify the resolution calculus. It is a consumer of `NormalForm` outputs, not a participant in rewrite rules.

---

## 12. Summary

### 12.1 The Stack

```
document
  → realize → close → normalize
  → NormalForm
  → SemanticIdentity (SID)       [canonical, structural, key-free]
  → CapabilityGrant              [delegated, scoped, epoch-bounded]
  → AdapterCredential            [domain-separated, ecosystem-specific]
  → IdentityDescriptor           [canonical, read-only, resolvable, non-authoritative]
  → IdentityProjection           [observational, non-authoritative]
```

### 12.2 The Laws

| Law | Statement |
|---|---|
| SID Determinism | Same inputs → same SID |
| Canonical Dependency | SID requires `NormalForm` as input |
| Schema Pinning | SID is schema-specific; schema change changes SID |
| Meaning Primacy | Identity is structural; keys are downstream |
| Canonical Encoding | Same logical SID inputs → same `SID_input_bytes` → same SID |
| Domain Tag | SID hash input is always prefixed with `"DBC-SID-1.1\0"` |
| Namespace | A bare digest without `sid:dbc:` or `fidx:` prefix is not a valid SID in text form |
| Capability Delegation | Authority requires a `CapabilityGrant` signed by the Governor |
| No Self-Authorization | A SID does not authorize itself; an actor cannot sign its own grant |
| Governor Chain | Every grant traces to a root trust anchor declared in the schema |
| Epoch-Boundedness | Grants are valid only within their declared epoch |
| Non-Confusion | A grant over `SID(N, σ1)` does not apply to `SID(N, σ2)` |
| Adapter Isolation | Domain-separated adapters are computationally independent |
| Non-Reversal | Adapter credentials cannot recover SID |
| Adapter Determinism | Same SID + domain label → same credential |
| Descriptor Non-Authority | `IdentityDescriptor` does not define SID, grants, or credentials |
| Descriptor Read-Only | Descriptors are produced by projection; never by direct write |
| Descriptor Determinism | Same governed state → same descriptor |
| Descriptor Epoch Validity | Descriptors from prior epochs must not be accepted as current |
| DID Projection Read-Only | DID document projection does not backflow into `IdentityDescriptor` or SID |
| Identity Projection Read-Only | Projection does not modify SID or credentials |
| No-Backflow | Nothing downstream influences the SID |

### 12.3 Acceptance Criteria

A conforming implementation of DBC-IDL-1.2 must satisfy:

- SID is derived only from `NormalForm` outputs
- SID derivation is deterministic and schema-pinned
- SID input bytes are encoded in the canonical TLV field order defined in §1.7
- SID hash input includes the domain tag `"DBC-SID-1.1\0"`
- SIDs in text form use the `sid:dbc:` or `fidx:<name>:` prefix; bare digests are rejected
- All adapter derivations use domain-separated HKDF
- No adapter label is shared across adapter types
- Capability grants are signed by the governor's adapter credential, not the actor's
- No capability grant is accepted without a valid and verifiable governor chain
- The actor presents the grant; the actor does not sign it
- Capability grants are epoch-bounded; expired grants are rejected
- Revocation records are checked before grant acceptance
- `IdentityDescriptor` is produced by projection from governed state, never by direct write
- `IdentityDescriptor` fields are in canonical key order; `descriptor_digest` is verified on receipt
- Descriptors from prior epochs are rejected
- DID document projections (`did:dbc:`) are re-derived on demand from the current descriptor
- Neither `IdentityDescriptor` nor any DID projection backflows into SID or governed state
- Identity projections do not feed back into SID, grants, adapters, or descriptors
- Adapter compromise in one domain does not affect other domains

---

## Changelog

| Version | Change |
|---|---|
| DBC-IDL-1.0 | Initial specification |
| DBC-IDL-1.1 | Fixed capability grant signer from actor to governor; added canonical SID encoding rule (§1.7) with TLV field order and domain tag; added SID prefix namespace (§1.8); updated law inventory; updated acceptance criteria |
| DBC-IDL-1.2 | Added `IdentityDescriptor` as Layer 4 (§4); defined canonical JSON form, field definitions, descriptor laws, registry-free resolution (§4.7), DID-compatible `did:dbc:` projection (§4.8), and DID registry comparison table (§4.9); updated doctrine; updated to five-object model (§6); added five descriptor laws to law inventory; updated acceptance criteria; renumbered §5–§12 |

---

*DBC-IDL-1.2 · DBC Identity Layer · Brian Thorne · bthornemail*
