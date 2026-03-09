# Reference Conformance Checklist (Minimal Cut)

Use this checklist to validate an independent implementation against this reference stack.

## Layer order

- [ ] Substrate (`NRR`) implemented before protocol/node/network/hub behavior.
- [ ] Protocol (`DBC + DBC-IDL`) computes meaning/identity without node/network coupling.
- [ ] Core executes protocol faithfully and persists canonical artifacts in substrate.
- [ ] Network performs semantic routing only (no semantic reinterpretation).
- [ ] Hub is projection-only inspection shell.

## Named law coverage

- [ ] Content-addressed blob storage and append-only log.
- [ ] Replay determinism and checkpoint replay equivalence.
- [ ] Stage-indexed protocol resolution and typed reject surfaces.
- [ ] SID derivation from canonical NormalForm with deterministic schema digest.
- [ ] Descriptor projection is non-authoritative and self-verifiable.
- [ ] Core validation failures are distinct from protocol rejects.
- [ ] HD-RPC `call(SID, stage)` forwards to Core semantics without reinterpretation.
- [ ] `adapter:ipv6` canonical, `adapter:ipv4` compatibility-only.
- [ ] Hub read panes are mutation-free; only resolve mutates canonical state.
- [ ] Capability verification outcomes are deterministic, typed, and evidence-carrying.
- [ ] Capability verification does not alter SID, descriptor identity, or NormalForm semantics.
- [ ] Capability-gated resolve and guarded adapter paths are deterministic across Core/Network/Hub.

## Required demos

- [ ] Hello Tetragrammatron: same meaning -> same NormalForm -> same SID -> same IPv6.
- [ ] Node pipeline: resolve -> persist -> descriptor -> adapter -> store/replay inspection.
- [ ] Minimal federation proof: routed call equals direct core call; unknown route typed failure.
- [ ] Capability demos:
  - valid delegation allows guarded action
  - expired chain denies guarded action
  - scope escalation denies guarded action

## Required negative proofs

- [ ] Malformed resolve request returns deterministic `invalid_request`.
- [ ] Invalid SID returns deterministic `invalid_sid`.
- [ ] Unknown SID route returns deterministic `route_not_found` or equivalent typed failure.
- [ ] Unsupported adapter returns deterministic `unsupported_adapter`.
- [ ] Capability malformed request returns deterministic `invalid_request` / `malformed_capability`.
- [ ] Invalid chain states return deterministic typed rejects (`broken_chain`, `invalid_signature`, `scope_escalation`, `epoch_expired`, etc.).

## Reproducibility

- [ ] Frozen demo snapshots checked into repo and verified by tests.
- [ ] Snapshot drift fails CI until explicitly reviewed.
- [ ] Fresh run from empty state reproduces demo snapshots.
- [ ] Bundle export/import preserves descriptor and route lookup behavior.

## Deferred (intentional)

- [ ] EVR doctrine expansion.
- [ ] Role-segmented hub.
- [ ] Peer discovery mesh / federation breadth.
- [ ] Embedded profile optimization policies.
