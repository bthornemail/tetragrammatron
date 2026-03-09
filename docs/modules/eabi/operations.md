# EABI Operation Inventory (Track G)

## Core

1. `resolve`
- payload: `canonical_input`, `schema_digest`, `target_stage`, optional `view_digest`
- context: optional `capability`, optional `federation`

2. `get-descriptor`
- payload: `sid_digest`, optional `format`
- context: optional `federation`

3. `verify-capability`
- payload: capability context payload
- context: optional `revocation`

4. `verify-revocation`
- payload: revocation verification payload

5. `derive-adapter`
- payload: `adapter_label`, `sid`
- context: optional `capability`

## Network

6. `routed-call`
- payload: `sid`, `target_stage`, optional `canonical_input`
- context: optional `capability`, optional `federation`

## Store/Evidence

7. `bundle-export`
- payload: optional `refs`, optional `include_log_segment`

8. `bundle-import`
- payload: `bundle`

9. `verify-store`
- payload: optional `refs`

## EVR

10. `event-batch-request`
- payload: optional `filter`, optional `limit`, optional `from_index`

11. `event-stream-item`
- stream framing operation for EVR items

## Envelope field order

- Invocation: `eabi_version`, `operation`, `context`, `payload`
- Success: `eabi_version`, `ok`, `operation`, `result`
- Error: `eabi_version`, `ok`, `operation`, `error`
- Error object: `code`, `details`, `message`
- Stream item: `eabi_version`, `item`, `sequence`, `stream`
