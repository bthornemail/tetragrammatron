# Capability Module Fixture Matrix (Track A)

This matrix defines the required fixture surface for runtime-complete capability verification.

## Golden fixtures

### G-01 Valid single grant

- One subject SID
- One governor anchor
- One actor grant
- In-epoch and in-scope
- Expected: `verified`

### G-02 Valid delegated chain

- Root governor delegates to intermediate governor
- Intermediate governor delegates to actor
- In-epoch and in-scope
- Expected: `verified`

### G-03 Valid bounded scope

- Grant chain valid
- Action and resource inside declared scope bounds
- Expected: `verified`

### G-04 Valid epoch-bound grant

- Grant chain valid and current epoch in range
- Expected: `verified`

### G-05 Valid adapter-guarded request

- Grant chain valid and adapter label within scope
- Expected: `verified`

### G-06 Valid chain with multiple intermediate governors

- Root delegates through intermediate governors to final actor
- Expected: `verified`

## Negative fixtures

### N-01 Broken chain

- Missing or unverifiable intermediate hop
- Expected: `broken_chain`

### N-02 Expired epoch

- Chain signatures valid but epoch outside validity window
- Expected: `epoch_expired`

### N-03 Wrong governor anchor

- Chain root/governor does not match accepted anchor context
- Expected: `invalid_governor`

### N-04 Scope mismatch

- Chain valid, requested action outside grant scope
- Expected: `scope_mismatch`

### N-05 Epoch not yet valid

- Grant chain valid but current epoch is before `not_before`
- Expected: `epoch_not_yet_valid`

### N-06 Adapter mismatch

- Chain valid but requested adapter is outside declared adapter scope
- Expected: `adapter_not_authorized`

### N-07 Malformed grant structure

- Grant object missing required fields or invalid epoch shape
- Expected: `malformed_capability`

### N-08 Identity mismatch

- Request subject does not match delegated subject in final grant
- Expected: `identity_mismatch`

### N-09 Unauthorized delegation depth

- Chain length exceeds configured delegation-depth limit
- Expected: `unauthorized_delegation_depth`

### N-10 Invalid signature

- Signature bytes do not match canonical grant payload
- Expected: `invalid_signature`

## Determinism fixtures

### D-01 Same chain -> same result

- Re-run identical valid chain input
- Expected: identical `valid` result shape

### D-02 Equivalent encoding -> same result

- Canonically equivalent grant encoding variants
- Expected: identical verification result and evidence

### D-03 Same invalid chain -> same reject

- Re-run identical invalid chain input
- Expected: identical reject kind and evidence

### D-04 Verification order independence

- Equivalent repeated evaluations preserve identical result/evidence
- Expected: stable deterministic envelope

## Integration fixtures

### I-01 Resolve with valid capability

- Required capability context is valid
- Expected: Core resolve succeeds

### I-02 Resolve with invalid capability

- Required capability context is invalid
- Expected: deterministic `capability_denied` host-validation failure

### I-03 Guarded adapter with valid capability

- Adapter guard request is authorized
- Expected: adapter derivation succeeds

### I-04 Guarded adapter with invalid capability

- Adapter guard request is unauthorized
- Expected: deterministic `adapter_not_authorized`

### I-05 Hub capability view equivalence

- Hub capability pane and Core verifier output are equivalent
- Expected: projection equality

## Fixture output requirements

Each fixture output must include:

- result kind (`verified` or typed reject)
- deterministic evidence object
- stable ordering for evidence keys/arrays where applicable
