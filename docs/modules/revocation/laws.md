# Revocation Module Laws (Track E)

## Purpose

Define deterministic, replayable authority withdrawal for delegated capability grants.

## Non-goals (for this module boundary)

- No SID or descriptor semantics changes.
- No canonical NormalForm changes.
- No online certificate infrastructure design.
- No vendor HSM abstraction.
- No mutable blacklist service model.
- No broad policy engine behavior.

## Boundary doctrine

Revocation withdraws delegated authority; it does not mutate identity, canonical meaning, or protocol truth.

## Core laws

### 1. Revocation-Is-Authority-Withdrawal Law

Revocation marks authority as invalid for targeted grants/chains under explicit rules. It does not alter subject identity.

### 2. Identity and Meaning Invariance Law

Revocation cannot alter SID derivation, descriptor identity, or canonical protocol outputs.

### 3. Explicit Targeting Law

Revocation must target explicit grant or chain references through canonical target fields.

### 4. Explicit Revoker Authority Law

Revocation records are valid only when issued by an authorized revoker for the target scope.

### 5. Effective-Epoch Law

Revocation effect is epoch-bounded and deterministic. Evaluation outside allowed epoch context yields typed outcomes.

### 6. Chain Propagation Law

If a revoked grant is an ancestor in a delegated chain, descendant authority validity is deterministically affected according to propagation rules.

### 7. Deterministic Verification Law

Same capability chain, same revocation set, same epoch context => same verification result.

### 8. Replayable Evidence Law

Revocation decisions must be reproducible from canonical records and replay context; no hidden mutable state is authoritative.

### 9. Typed Outcome Law

Revocation verification outcomes are typed, deterministic, and evidence-carrying.

### 10. EVR Observability Law

Revocation checks/applications are observable through EVR evidence events, but EVR does not define revocation truth.

## Canonical structures (module-level)

### RevocationRecord (minimum)

- `version`
- `record_id`
- `target_kind` (`grant|chain`)
- `target_ref`
- `revoker_id`
- `effective_epoch`
- `scope`
- `witness`

### RevocationResult (typed)

- `not_revoked`
- `revoked`
- `unauthorized_revoker`
- `malformed_revocation`
- `target_not_found`
- `revocation_out_of_scope`
- `unsupported_revocation_version`

## Layer interaction boundaries

### Capability boundary

Capability verification incorporates revocation status as an authority validity input only.

### Core boundary

Core executes revocation verification and returns typed deterministic outcomes without semantic invention.

### EVR boundary

EVR carries observational evidence for revocation record/check/apply/reject outcomes.

### Hub boundary

Hub renders revocation state and evidence as projection-only views.
