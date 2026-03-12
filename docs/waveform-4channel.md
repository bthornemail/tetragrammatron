# Waveform 4-Channel

`waveform-4channel` is the geometric bridge layer between the separator-based control surface and EABI.

## Boundary

This module maps structural separators to geometric and channel interpretation.

- it does not define canonical semantic truth
- it does not redefine EABI invocation semantics
- it does not redefine ABI result semantics

## Layer placement

```text
control surface -> waveform-4channel -> bridge -> EABI -> ABI
```

Meaning:

- control surface: framing and navigation only
- waveform-4channel: geometric interpretation only
- bridge: operation selection and envelope construction only
- EABI: invocation boundary
- ABI: canonical semantic truth

## Public surface

Implemented module:

- [waveform-4channel.mjs](/home/main/devops/tetragrammatron/src/geometry/waveform-4channel.mjs)

Exports:

- channels: `binary`, `decimal`, `hex`, `sign`
- polytopes and dual map
- expansion helpers: `createExpansion`, `rational`
- geometric helpers: `analyzeExpansion`, `distance`, `midpoint`, `difference`, `containment`
- transforms: `project`, `dual`, `compose`, `unleash`
- experimental Unicode helpers: `encodeSurrogate`, `decodeSurrogate`

## Experimental Unicode note

Surrogate encoding and decoding are implemented as experimental bridge metadata only.

- they are not part of ABI conformance
- they are not part of EABI conformance
- they are not release-critical semantic truth

## Bridge contract

The bridge may prefix a control-frame payload with a two-character surrogate pair.

- if the payload starts with a valid surrogate pair, the bridge derives channel and geometric metadata from it
- otherwise the bridge falls back to the structural separator (`FS/GS/RS/US`)
- the remaining payload is UTF-8 JSON for a repo-valid EABI invocation payload
