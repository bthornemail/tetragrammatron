# Capability Module Laws (Track A)

## Purpose

Define the canonical authority-delegation model and verification semantics for capability grants.

## Non-goals (for this module boundary)

- No wallet or adapter integration behavior.
- No UI policy behavior.
- No federation/discovery policy behavior.
- No device bootstrap workflow behavior.

## Boundary doctrine

**Capabilities delegate authority; they do not create identity, alter canonical meaning, or redefine descriptors.**

## Core laws

### 1. Authority is delegated

A capability is valid only when authority is delegated through a verifiable chain rooted in an accepted governor anchor.

### 2. Capability is not identity

Capability grants never define SID. SID remains derived from canonical NormalForm only.

### 3. Capability does not alter SID

Verification outcomes cannot mutate, replace, or reinterpret subject SID.

### 4. Capability does not alter descriptor semantics

Capabilities may govern action authorization, but never redefine descriptor truth.

### 5. Grant chain validity

A chain is valid iff each hop is cryptographically/verifiably bound, ordered, and rooted at an accepted authority.

### 6. Epoch and scope bounds

A grant is valid only within its epoch and declared scope. Outside epoch/scope is non-valid.

### 7. Deterministic verification

For identical chain input, anchors, epoch context, and scope context, verification result must be identical.

### 8. Typed rejection

Verification failure is typed, deterministic, and evidence-carrying; no generic or ambiguous failure surface.

## Verification result taxonomy (runtime-complete)

- `verified`
- `invalid_request`
- `malformed_capability`
- `unsupported_capability_version`
- `broken_chain`
- `invalid_signature`
- `invalid_governor`
- `wrong_governor`
- `scope_escalation`
- `scope_mismatch`
- `adapter_not_authorized`
- `epoch_not_yet_valid`
- `epoch_expired`
- `identity_mismatch`
- `unauthorized_delegation_depth`

## Evidence requirements (shape-level)

Every non-valid result must include deterministic evidence fields sufficient to audit why verification failed. Evidence schema details are defined by fixtures first.
