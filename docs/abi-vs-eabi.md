# ABI vs. EABI

**docs/abi-vs-eabi.md**  
Author: Brian Thorne · [github.com/bthornemail](https://github.com/bthornemail)  
See also: [Track F · ABI](./tracks/track-f-abi.md) · [Track G · EABI](./tracks/track-g-eabi.md)

---

## One-Sentence Boundary

> **ABI defines the canonical semantic structures. EABI defines how those structures cross execution boundaries. EABI never redefines ABI.**

That is the entire distinction.

**Track F (ABI)** establishes the language-neutral semantic contract: canonical structures, typed result envelopes, deterministic ordering, and evidence legality.

**Track G (EABI)** establishes the execution boundary that carries those structures across hosts, processes, runtimes, and transports without semantic loss.

---

## Short Version

### ABI — what the system means

ABI specifies:
- canonical semantic structures
- typed results and rejects
- deterministic ordering and encoding
- evidence requirements
- closed status taxonomies

If two implementations conform to ABI, they agree on what a `ResolveResult` is, what a `CapabilityVerificationResult` is, what an `EVREvent` is, what a `ConvergenceWitness` is.

> **ABI describes canonical semantic structures and outcomes, not how a runtime hosts or transports them.**

### EABI — how environments invoke and exchange that meaning

EABI specifies:
- invocation envelopes
- success and error envelopes
- context passing and normalization
- operation versioning
- event stream framing
- bundle and artifact handoff framing
- execution error taxonomy

If two implementations conform to EABI, they agree on how to call `resolve`, how to request `get-descriptor`, how to perform a `routed-call`, how to import and export bundles, and how execution errors differ from semantic failures.

> **EABI defines how canonical ABI structures cross execution boundaries. It does not define new semantics.**

---

## Practical Distinction

The `waveform-4channel` module is a semantic bridge layer between structural control framing and EABI invocation. It is neither ABI nor EABI and must not redefine canonical semantic truth or execution-boundary semantics.

**Put something in ABI when it answers:**
- What is the canonical shape of this semantic object?
- What statuses are legal?
- What evidence is required?
- What ordering makes this deterministic?
- What result must every implementation reproduce?

**Put something in EABI when it answers:**
- How do I invoke this operation?
- What envelope carries the request?
- What envelope carries the response?
- How are context blocks passed?
- How do bundles, refs, and events cross a runtime boundary?
- What is an execution failure vs. a semantic failure?

---

## Examples

### Example 1 — Resolve

**ABI side** defines the semantic structures: `ResolveCall`, `ResolveResult`, `RejectEnvelope`, stage/value-kind legality, canonical ordering, and the typed reject taxonomy.

**EABI side** defines the invocation boundary: the invocation envelope for `"resolve"`, the success envelope (`ok: true`), the execution failure envelope (`ok: false`), required context legality, and operation versioning rules.

---

### Example 2 — Capability

**ABI side** defines the semantic structure: `CapabilityVerificationResult`, the closed status set (`valid` · `expired` · `revoked` · `invalid_signature` · `broken_chain` · `schema_mismatch`), and the evidence shape required for each status.

**EABI side** defines the invocation boundary: the `"verify-capability"` operation, how `capability_grant` is passed, how revocation context is passed, and how a malformed invocation becomes an execution failure rather than a semantic failure.

---

### Example 3 — EVR

**ABI side** defines the semantic event: `EVREvent`, family/kind legality, required evidence shape per event kind, and canonical ordering of event fields.

**EABI side** defines the execution framing: `event-batch-request`, `event-batch-response`, `event-stream-item`, and the stream/session framing around verbatim EVR events. EVR events are carried verbatim inside the framing; the framing never alters event content.

---

### Example 4 — Federation

**ABI side** defines the semantic structures: `FederationAnnouncement`, `RouteSet`, `ArbitrationResult`, `ConvergenceWitness`, `DivergenceWitness`.

**EABI side** defines the execution boundary: the `"routed-call"` invocation, federation context passing, bundle and artifact handoff, and the execution-boundary framing for route lookup and forwarding.

---

## The Critical Failure Distinction

This is the most common point of confusion.

### Semantic failure (ABI level)

A semantic failure means the call successfully reached the semantic layer and the semantics returned a typed non-success. The execution boundary worked correctly.

Examples: resolve reject, capability expired, descriptor not found, federation divergence.

These appear inside a **successful EABI response** (`ok: true`):

```json
{
  "eabi_version": "1.0",
  "ok": true,
  "operation": "resolve",
  "result": {
    "reject_kind": "RejectRealize",
    "reject_code": "invalid_symbol",
    "canonical_evidence": { "detail": "relation 'unknownRel' not admitted" }
  }
}
```

`ok: true` means the EABI layer succeeded. The ABI result inside `result` carries the semantic outcome, which may itself be a typed non-success.

### Execution failure (EABI level)

An execution failure means the invocation never reached the semantic layer. The envelope, context, or operation surface failed first.

Examples: malformed envelope, missing required field, unsupported operation, unsupported version, malformed bundle.

These return an EABI error envelope (`ok: false`):

```json
{
  "eabi_version": "1.0",
  "ok": false,
  "operation": "resolve",
  "error": {
    "code": "invalid_envelope",
    "details": {},
    "message": "missing required field: schema_digest"
  }
}
```

### Non-negotiable rule

> **An EABI host must never translate an ABI semantic failure into an EABI execution error.**

A resolve reject is not a malformed envelope. A capability expiry is not an unsupported operation. If the semantic layer produced a typed result, that result goes inside `ok: true` regardless of whether it is a success or a failure in the semantic sense.

---

## The Three-Question Test

When deciding whether something belongs in ABI or EABI:

**1. Does it change what the system means?**  
If yes -> **ABI** (or lower, in the protocol/substrate layer).

**2. Does it change how the system is invoked or how structures cross a boundary?**  
If yes -> **EABI**.

**3. Does it change where the system runs?**  
If yes -> **EABI** or out of scope for both layers (transport, deployment, process management).

If none of the three apply, the thing does not belong in either ABI or EABI.

---

## Compact Comparison Table

| ABI | EABI |
|---|---|
| Canonical semantic structures | Canonical execution boundary |
| What the system means | How environments invoke and exchange it |
| Typed semantic results and rejects | Typed execution error envelopes |
| Evidence legality (EVR and capability) | Context passing and operation framing |
| Canonical ordering of semantic structures | Canonical ordering of invocation and result envelopes |
| Closed status taxonomies | Closed execution error codes |
| Independent semantic targetability | Independent runtime invocability |
| `src/abi/` | `src/eabi/` |
| `v1.1.0-abi` | `v1.2.0-eabi` |

---

## Layer Gate — PR Rule

For every pull request that touches `src/abi/`, `src/eabi/`, or `docs/modules/abi|eabi/`, the author must answer:

```text
Layer gate check:

1. Does this PR change what the system means?
   If yes — it belongs in ABI or below. Does it touch src/abi/ or lower? [ ]

2. Does this PR change how the system is invoked or how structures cross a boundary?
   If yes — it belongs in EABI. Does it touch src/eabi/? [ ]

3. Does this PR introduce transport-specific behavior, process management,
   auth/session design, or deployment concerns?
   If yes — it does not belong in ABI or EABI. Remove it before merging. [ ]

4. Does this PR translate an ABI semantic failure into an EABI execution error,
   or an EABI execution error into an ABI semantic status?
   If yes — this is a conformance violation. Block merge. [ ]
```

A PR that cannot answer the first two questions clearly has not identified which layer it belongs to and should not merge until it can.

A PR that triggers question 4 is always a conformance violation. There are no exceptions.

---

## Contributor Rule of Thumb

> **If you are defining meaning, statuses, evidence, or canonical semantic structures — you are working in ABI.**
>
> **If you are defining invocation envelopes, context passing, bundle handoff, event framing, or execution errors — you are working in EABI.**

---

## Pin This

If you pin one paragraph in the repo root or architecture docs:

> ABI defines what the system means. EABI defines how environments invoke and exchange that meaning. ABI fixes canonical semantic structures; EABI carries those structures across execution boundaries without changing their meaning.

That paragraph will prevent more architectural drift than any linter.

---

## References

- [Track F · ABI](./tracks/track-f-abi.md)
- [Track G · EABI](./tracks/track-g-eabi.md)
- [ARCHITECTURE.md](../ARCHITECTURE.md)
- [DBC-INDEX.md](../DBC-INDEX.md)

---

*docs/abi-vs-eabi.md · Tetragrammatron · Brian Thorne · bthornemail*
