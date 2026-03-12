# Control Surface

ASCII Information Separators provide a deterministic four-level control hierarchy for Tetragrammatron framing.

## Boundary

This control surface is a framing and navigation layer only. It does not redefine ABI, EABI, protocol semantics, identity, authority, or conformance.

## Control Byte Table

| Byte | Hex | Name | Scope | Channel | Level |
|---|---|---|---|---|---|
| `SP` | `0x20` | Space | identity | none | 0 |
| `US` | `0x1F` | Unit Separator | unit | 0 | 1 |
| `RS` | `0x1E` | Record Separator | record | 1 | 2 |
| `GS` | `0x1D` | Group Separator | group | 2 | 3 |
| `FS` | `0x1C` | File Separator | file | 3 | 4 |
| `ESC` | `0x1B` | Escape | control entry | n/a | n/a |

Use names plus hex in implementation docs. Do not rely on caret mnemonics.

## Mode Semantics

### Data mode

In data mode, `FS`, `GS`, `RS`, and `US` are structural delimiters:

- `FS` separates file contexts
- `GS` separates group scopes
- `RS` separates records or line-like units
- `US` separates atomic units or tokens

### Control mode

In control mode, `ESC` promotes the following separator from delimiter to instruction selector.

Canonical compact frame:

```text
[ESC] [separator] [payload_length] [payload bytes...]
```

Where:

- `separator` must be one of `FS/GS/RS/US`
- `payload_length` is one byte for compact frames (`0-254`)
- `0xFF` in the length slot means an extended 4-byte payload length follows
- `payload` is channel-specific data

## Nesting Law

The hierarchy is strict:

```text
FS > GS > RS > US > SP
```

Meaning:

- file contains groups
- group contains records
- record contains units
- unit contains atomic payload

## Canonical Examples

### Data hierarchy

```text
alpha US beta RS gamma GS delta FS omega
```

Parses as:

- file
- group
- record
- units: `alpha`, `beta`
- record
- units: `gamma`
- group
- record
- units: `delta`
- group
- record
- units: `omega`

### Selector sequence

```text
ESC FS GS RS US
```

Interprets as a descending navigation path:

- file
- group
- record
- unit

### Escaped control frame

```text
ESC FS 0x07 resolve
```

Interprets as:

- enter control mode
- select file-level channel (`channel 3`)
- read seven bytes of payload
- payload = `resolve`

## AST / LSP Mapping

| Separator | AST role | LSP role |
|---|---|---|
| `US` | token or lexeme | character position |
| `RS` | expression or statement | line or range |
| `GS` | block or scope | document region |
| `FS` | module or file | file boundary |

This allows file-to-token traversal without inventing a second hierarchy.

## Implementation

Reference implementation:

- parser and encoder: [surface.mjs](/home/main/devops/tetragrammatron/src/control/surface.mjs)
- tests: [control-surface.test.mjs](/home/main/devops/tetragrammatron/tests/control-surface.test.mjs)

Implemented primitives:

- separator metadata lookup
- escaped control frame encode/decode
- selector-sequence parsing
- hierarchical data-mode splitting for `FS/GS/RS/US`

## Bridge Layer

The 4-channel module sits above this control surface as a semantic bridge layer.

- control surface provides structural separators and escaped frame parsing
- `waveform-4channel` maps structural separators to geometric and channel interpretation
- the bridge maps interpreted channel selection into EABI invocation

This layer does not define canonical semantic truth and does not redefine EABI invocation or ABI result semantics.
