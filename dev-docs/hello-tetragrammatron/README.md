# Hello Tetragrammatron

A classic minimal demo for the Tetragrammatron loop:

```text
meaning -> canonical form -> SID -> address -> computation
```

## Files

- `hello.js` — two independent nodes converge on the same NormalForm, SID, IPv6, and optional IPv4 compatibility hint.
- `call.js` — tiny HTTP version showing `call(SID, stage)` routing to a node and fetching by SID.
- `package.json` — convenience scripts.

## Why IPv4 is present

`adapter:ipv6` should remain the canonical network projection.
`adapter:ipv4` is included here only as a compatibility projection for environments where IPv6 is unavailable or blocked.
It is not authoritative and should not be treated as part of identity.

## Run

```bash
npm run hello
npm run call
```

## What `hello.js` proves

- same document
- same normalized form
- same SID
- same derived IPv6
- same derived IPv4 compatibility hint
- no registry or coordination required

## What `call.js` proves

- you can compute a SID from canonical meaning
- you can derive transport hints from the SID
- you can route `call(SID, stage)` to a node
- the node returns canonical state for that SID

## Expected shape

```text
DOCUMENT
{ "schema": "demo.hello", "name": "world" }

NODE A
... sid:dbc:...

NODE B
... sid:dbc:...

CONVERGENCE
same normal form: true
same SID:         true
same IPv6:        true
same IPv4 hint:   true
```
