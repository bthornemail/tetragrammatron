# ABI Implementation Guide

## Required for conformance

1. Produce ABI structures with required fields and closed status sets.
2. Validate EVR event evidence legality by EventKind.
3. Enforce canonical ordering for fields/sequences.
4. Produce canonical encoding deterministically.

## Not required for ABI conformance

- Reference implementation internal architecture
- Process model, FFI, sockets, CLI behavior, filesystem layout
- Runtime packaging/deployment choices

## Two-step conformance path

1. Structure conformance
- Validate field presence/types/status legality/evidence shape.

2. Encoding conformance
- Prove byte-identical canonical JSON for equivalent semantic inputs.

## Migration guidance

- Map internal results into ABI envelopes using explicit adapters.
- Preserve original internal statuses in ABI evidence when taxonomy compression is required.
- Do not change baseline semantics to satisfy ABI shape.
