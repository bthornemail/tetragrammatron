# Protocol Laws (Phase 2 Minimal DBC + DBC-IDL)

## DBC

- Stage values are explicit: `Realized`, `Closed`, `Normalized`, `Projected`.
- `resolveTo(stage, input)` is deterministic and stage-indexed.
- `NormalForm` is canonical (sorted, deduplicated, stable JSON encoding).
- Rejects are typed values with evidence (`reject_kind`, `reject_code`, `evidence`).
- Stage envelopes are explicit on every result (`envelope.stage`, `envelope.ok`).
- Deterministic reject families include `RejectNormalize` and `RejectProject`.

## DBC-IDL

- SID is derived only from canonical `NormalForm`.
- SID derivation uses explicit domain tag and canonical byte encoding.
- Schema digest derivation is explicit and deterministic.
- IdentityDescriptor is a non-authoritative projection and must self-verify.

## Boundaries

- No NRR coupling in protocol core.
- No HTTP endpoints.
- No network adapters.
- No capability/adapters beyond minimal descriptor projection helper.
