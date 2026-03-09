# EABI Release Checklist

## G1-G2 docs

- [x] Laws complete with ABI/EABI boundary sentence.
- [x] Operation inventory complete.
- [x] Fixture matrix complete (13/7/3).

## G3-G6 runtime

- [x] `src/eabi/schema.mjs` complete.
- [x] `src/eabi/validate.mjs` complete.
- [x] `src/eabi/encode.mjs` complete.
- [x] `src/eabi/decode.mjs` complete.
- [x] `src/eabi/errors.mjs` complete.
- [x] `src/eabi/operations.mjs` complete.
- [x] `src/eabi/host.mjs` complete.
- [x] `src/eabi/events.mjs` complete.
- [x] `src/eabi/bundle.mjs` complete.

## G7-G9 conformance

- [x] EABI fixture sets frozen.
- [x] EABI tests pass for schema/host/network/bundle/events.
- [x] EABI conformance snapshots pass.
- [x] EABI demo scripts reproducible.

## G10 release

- [x] Module manifest updated to `1.2.0-eabi`.
- [x] Changelog includes Track G section.
- [x] Release conformance includes EABI statement.
- [x] README includes EABI module + demos.
