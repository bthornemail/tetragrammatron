# Unicode Geometry

Mechanical Unicode encoding for Tetragrammatron's Fano and wave carrier layer.

## Boundary

This module is adjacent to `control` and `waveform-4channel`.

- mechanical encoding only
- configurable registries only
- no ABI dependency
- no EABI dependency
- no semantic authority claims

## Public contract

- `channel`: `0..3`
- `point`: `1..7`
- `wave`: `0..15`
- `event`: `0..63`
- `variation`: `0..31`

## Main exports

- `encodeSurrogate()`
- `decodeSurrogate()`
- `DEFAULT_FANO_POINTS`
- `DEFAULT_WAVES`
- `createPointRegistry()`
- `createWaveRegistry()`
- `createLocalRegistry()`
- `createFederationRegistry()`
- `createCanonicalRegistry()`
