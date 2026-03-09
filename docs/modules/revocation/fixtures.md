# Revocation Module Fixture Matrix (Track E)

This matrix defines required revocation coverage before runtime integration is considered complete.

## Golden fixtures

### R-G01 Direct grant revoked by valid authority

- valid revocation record targets a grant
- authorized revoker
- expected: `revoked`

### R-G02 Delegated chain invalidated by revoked ancestor

- ancestor grant revoked
- descendant chain verification affected deterministically
- expected: `revoked`

### R-G03 Revocation effective within valid epoch

- revocation record issued and evaluated in effective window
- expected: deterministic application

### R-G04 Non-revoked chain remains valid

- no applicable revocation records
- expected: `not_revoked`

## Negative fixtures

### R-N01 Malformed revocation record

- missing/invalid required fields
- expected: `malformed_revocation`

### R-N02 Unauthorized revoker

- revoker lacks authority for target
- expected: `unauthorized_revoker`

### R-N03 Revocation target mismatch

- target reference does not match checked chain/grant
- expected: `target_not_found`

### R-N04 Stale/invalid revocation witness

- invalid witness structure or invalid integrity witness
- expected: `malformed_revocation`

### R-N05 Revocation out of scope

- revocation scope does not cover checked target context
- expected: `revocation_out_of_scope`

### R-N06 Conflicting revocation records

- duplicate/conflicting records for same target in same scope/epoch context
- expected: deterministic typed outcome under canonical record ordering (`revoked` or typed reject)

## Determinism fixtures

### R-D01 Same revocation inputs -> same result

- repeated evaluation over identical chain + revocation set + epoch
- expected: identical result and evidence schema

### R-D02 Equivalent encoding -> same result

- canonical-equivalent revocation record encodings
- expected: identical result and evidence schema

### R-D03 Same invalid revocation -> same typed reject

- repeated invalid record evaluation
- expected: identical reject kind/evidence schema

## Integration fixtures

### R-I01 Verify-capability reflects revocation

- capability verification includes revocation records
- expected: revocation-aware typed result

### R-I02 Guarded resolve denied after revocation

- previously valid delegated path becomes invalid post-revocation
- expected: deterministic deny

### R-I03 Routed call respects revocation

- federation-routed capability check includes revocation state
- expected: deterministic deny/success consistent with direct path

### R-I04 Hub revocation inspection equals Core result

- Hub projection of revocation status matches Core verification/evidence

## Fixture output requirements

Each fixture output must include:

- revocation result kind
- deterministic evidence object
- stable ordering for evidence keys/arrays where applicable
