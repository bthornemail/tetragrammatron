# EABI Implementation Guide

## Mandatory EABI host requirements

- Support canonical invocation/success/error envelopes.
- Preserve ABI payload structures without mutation.
- Separate semantic failures from execution failures.
- Implement context normalization rules.
- Implement deterministic envelope encoding.

## Optional host differences

- Internal architecture/runtime language
- Transport binding choice (stdin/stdout, HTTP body, socket, WASM boundary)
- Process lifecycle and deployment model

## Operation support

Required baseline operations:
- `resolve`
- `get-descriptor`
- `verify-capability`
- `derive-adapter`
- `routed-call`
- `bundle-export`
- `bundle-import`
- `verify-store`
- `event-batch-request`

Optional stream helper:
- `event-stream-item`

## Conformance path

1. Structure conformance
- Envelope validation and operation routing rules pass.

2. Behavior conformance
- EABI call semantics match direct host semantics.

3. Encoding conformance
- Canonical envelope encoding is deterministic.
