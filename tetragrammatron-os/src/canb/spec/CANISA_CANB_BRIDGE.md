# CAN-ISA ⇄ CANB Bridge (A)

This document defines **how higher-level CAN-ISA / Origami Fold VM operations**
map onto the **minimal CANB v1 primitive set** implemented by:

- `src/canasm/seed/canasm0.c` (assembler)
- `src/canb/wasm/canvm_wasm.c` (reference VM)

This is intentionally *lossy*: CANB is the **transport + replay core**;
higher semantics (polynomials, folds, proofs) live *above* as **macros**.

## 1. Canonical semantic atoms (8‑tuple keywords)

We avoid symbolic glyphs. The normalized 8 keywords are:

- `accept`
- `alphabet`
- `left`
- `right`
- `delta`
- `start`
- `state`
- `reject`

These keywords appear as **atoms** in CANB and as **node labels** in the Fano renderer.

## 2. CANB primitives (the only opcodes in the bootstrap VM)

| Primitive | Meaning | Notes |
|---|---|---|
| `ATOM <id>` | Push atom-id on stack | `id` is an index into the CANB atom table |
| `EDGE` | Pop `to`, pop `from`, add directed edge `from → to` | Pure relation, no numbers exposed |
| `PROJ_FANO` | Emit a *projection event* for visualization | Does **not** change the relation graph |
| `HALT` | Stop execution | |

These are enough to encode:
- a **relation algebra** (atoms + directed edges)
- a **projection layer** (events for SVG/Canvas visualization)
- a **portable replay trace** (deterministic)

## 3. Macro layer: Origami / fold operations

Higher-level instructions expand into a *small, checkable set* of primitive sequences.

### 3.1 `CANON` (canonicalize)

Goal: produce a canonical ordering / stable view without relying on numeric indices.

CANB cannot reorder graph storage; instead `CANON` expands to:

1) Emit a projection barrier event so all observers see the same **phase boundary**.

```canasm
PROJ_FANO
```

Optional (when you implement graph hashing later): emit a `CANON_HASH` event
as metadata on the event stream.

### 3.2 `MEET` (GCD / meet)

In the full system, `MEET` is polynomial GCD / constraint intersection.

In the bootstrap bridge, `MEET(a,b) -> m` is represented as:

- introduce a derived atom `meet:<a>:<b>`
- relate operands into the meet node

```canasm
ATOM a
ATOM meet:a:b
EDGE          ; a → meet:a:b
ATOM b
ATOM meet:a:b
EDGE          ; b → meet:a:b
PROJ_FANO
```

This preserves *structure* (a meet exists) without yet computing algebra.

### 3.3 `JOIN` (LCM / join)

Similarly, `JOIN(a,b) -> j` is represented as `join:<a>:<b>` with operand edges.

```canasm
ATOM a
ATOM join:a:b
EDGE
ATOM b
ATOM join:a:b
EDGE
PROJ_FANO
```

### 3.4 `FOLD_A6` (origami axiom 6 placeholder)

Axiom 6 is the “global constraint intersection”.

Bootstrap representation:

- create `fold:A6:<seed>`
- connect the **seven** Fano-point atoms (or your chosen 7) into it
- emit `PROJ_FANO`

```canasm
; assume atoms p0..p6 exist
ATOM p0  ATOM fold:A6:seed EDGE
ATOM p1  ATOM fold:A6:seed EDGE
ATOM p2  ATOM fold:A6:seed EDGE
ATOM p3  ATOM fold:A6:seed EDGE
ATOM p4  ATOM fold:A6:seed EDGE
ATOM p5  ATOM fold:A6:seed EDGE
ATOM p6  ATOM fold:A6:seed EDGE
PROJ_FANO
```

When you later implement real polynomial / incidence solving,
this macro becomes the one place you “upgrade” behavior.

## 4. Invariants for the bridge

Even in the bootstrap VM, the bridge MUST preserve:

- **No numeric indices in source**: assembler accepts labels/atoms, not raw ids.
- **Determinism**: same input → same CANB bytes.
- **Projection is non-semantic**: `PROJ_FANO` does not mutate the relation graph.
- **Pure relations**: all semantics are edges between atoms.

## 5. Why this is “A then B”

A = define the bridge so semantics can evolve without rewriting the transport.
B = implement toolchain (assembler/disassembler) that moves those bytes.
