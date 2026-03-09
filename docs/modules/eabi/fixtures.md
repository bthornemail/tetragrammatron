# EABI Fixture Matrix (Track G)

## Golden (13)

- EABI-G-01 `resolve` success
- EABI-G-02 `resolve` semantic reject in `ok: true`
- EABI-G-03 `get-descriptor` found
- EABI-G-04 `get-descriptor` semantic not-found
- EABI-G-05 `verify-capability` valid
- EABI-G-06 `verify-capability` expired/revoked typed semantic failure
- EABI-G-07 `verify-revocation`
- EABI-G-08 `derive-adapter`
- EABI-G-09 `routed-call`
- EABI-G-10 `bundle-export`
- EABI-G-11 `bundle-import`
- EABI-G-12 `event-batch-request`
- EABI-G-13 `event-stream-item`

## Negative (7)

- EABI-N-01 malformed envelope
- EABI-N-02 unsupported version
- EABI-N-03 missing required field
- EABI-N-04 unknown operation
- EABI-N-05 invalid artifact reference
- EABI-N-06 malformed context
- EABI-N-07 malformed bundle

## Determinism (3)

- EABI-D-01 same resolve invocation => same response envelope
- EABI-D-02 same bundle export request => same framing result shape
- EABI-D-03 same invalid invocation => same execution failure envelope
