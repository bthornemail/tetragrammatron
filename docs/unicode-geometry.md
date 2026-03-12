# Tetragrammatron Unicode Geometry

`unicode-geometry` is the mechanical Unicode carrier layer for the separator-based control surface and the 4-channel bridge.

## Boundary

This module maps numeric channel, point, wave, and event coordinates to surrogate pairs and provides configurable registry helpers.

- it is mechanical only
- it does not define ABI semantic truth
- it does not define EABI invocation semantics
- it does not make PUA or SPUA mappings canonical

## Layer placement

```text
control surface -> waveform-4channel -> unicode-geometry -> bridge
```

Meaning:

- control surface: framing and navigation only
- waveform-4channel: geometric interpretation only
- unicode-geometry: deterministic Unicode carrier mapping only
- bridge: adapter between control framing and Unicode carrier atoms

ABI and EABI remain authoritative for semantic truth and invocation behavior.

## Public numeric contract

- `channel`: `0..3`
- `point`: `1..7`
- `wave`: `0..15`
- `event`: `0..63`
- `variation`: `0..31`

## Encoding model

High surrogate:

```text
high = D800 + (channel * 256) + ((point - 1) * 32) + variation
```

Low surrogate:

```text
low = DC00 + (wave * 64) + event
```

Bit layout:

```text
High: [channel:2][point-1:3][variation:5]
Low:  [wave:4][event:6]
```

This yields a deterministic 20-bit carrier space per surrogate pair.

## Example registries

The default registries are examples, not frozen truth.

Fano points:

- `1 -> Metatron`
- `2 -> Solomon`
- `3 -> Solon`
- `4 -> ʿAsabiyyah`
- `5 -> Enoch`
- `6 -> Speaker`
- `7 -> Genesis`

Waves:

- `0 -> Wave16`
- `...`
- `15 -> Wave31`

## Private-use helpers

The module provides utility registry helpers for:

- Plane 0 PUA: `U+E000..U+F8FF`
- SPUA-A: `U+F0000..U+FFFFD`
- SPUA-B: `U+100000..U+10FFFD`

These helpers are local/federated storage utilities only. They do not define canonical mappings.

## Example

```js
import { decodeSurrogate, encodeSurrogate } from '../src/unicode-geometry/index.mjs';

const pair = encodeSurrogate({
  channel: 1,
  point: 2,
  wave: 0,
  event: 5,
  variation: 0,
});

const atom = decodeSurrogate(pair);
```

For that example:

- high surrogate = `U+D820`
- low surrogate = `U+DC05`

## Relationship to waveform-4channel

`waveform-4channel` remains the coordinate-oriented geometric layer.

`unicode-geometry` is the explicit carrier layer when the caller wants direct control over:

- channel
- point
- wave
- event
- variation

Use `waveform-4channel` for geometric transforms.
Use `unicode-geometry` for mechanical Unicode encoding.
