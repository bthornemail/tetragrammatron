# Capability Module Fixture Matrix (Track A)

This matrix defines the required fixture surface before runtime verifier implementation.

## Golden fixtures

### G-01 Valid single grant

- One subject SID
- One governor anchor
- One actor grant
- In-epoch and in-scope
- Expected: `valid`

### G-02 Valid delegated chain

- Root governor delegates to intermediate governor
- Intermediate governor delegates to actor
- In-epoch and in-scope
- Expected: `valid`

### G-03 Valid bounded scope

- Grant chain valid
- Action and resource inside declared scope bounds
- Expected: `valid`

## Negative fixtures

### N-01 Broken chain

- Missing or unverifiable intermediate hop
- Expected: `broken_chain`

### N-02 Expired epoch

- Chain signatures valid but epoch outside validity window
- Expected: `expired`

### N-03 Wrong governor

- Chain root/governor does not match accepted anchor context
- Expected: `wrong_governor`

### N-04 Scope mismatch

- Chain valid, requested action outside grant scope
- Expected: `scope_mismatch`

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

## Fixture output requirements

Each fixture output must include:

- result kind (`valid` or typed reject)
- deterministic evidence object
- stable ordering for evidence keys/arrays where applicable
