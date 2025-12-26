Cool — here are **1, 2, 3, 4**, and then **5** in order, written so you can drop them straight into your repo as `dev-docs/` RFC-style pieces (or merge into RFC-0012/0013 later).

---

## 1) Opcode IR Spec (pure relational, no encoding)

### 1.1 Core idea

An “opcode” is **not a number** and **not a byte**. It is a **named relational transformation** over a state graph.

### 1.2 Minimal IR types

**Entities**

* **Atom**: an indivisible symbol (device, register, node, label, line, point, fold)
* **Rel**: a binary relation `Rel ⊆ Atom × Atom`
* **State**: a record of named relations

**State shape (canonical)**

* `R0` = `NOTICE` relation set (observables)
* `R1` = `ADJ` (adjacency)
* `R2` = `CONTAIN` (containment)
* `R3` = `ALIGN` (alignment / orientation)
* `R4` = `TRACE` (event stream edges)
* `R5` = `PROOF_HOOK` (obligations + witnesses)
* `R6..R7` optional derived lenses (never stored)

**Opcode IR**
An opcode is:

* `name : Atom`
* `read : Set RelationName` (which relations it may read)
* `write : Set RelationName` (which relations it may modify)
* `guard : State → Bool` (must hold before applying)
* `effect : State → State` (pure)
* `canon : State → State` (optional closure; must be idempotent)

**Execution**
Program = list of opcode applications, each possibly with symbolic operands:

* `APPLY(op, args)` where args are **atoms**, not indices.

### 1.3 Determinism contract

* `effect` is pure.
* If `canon` exists: `canon(canon(s)) = canon(s)` must hold.
* `guard(s)=false` ⇒ opcode yields `REJECT` event, state unchanged.

---

## 2) Canonical UTF-8 Token Set (relations + traversals)

### 2.1 Token principles

* Tokens are UTF-8 strings.
* Tokens are **case-stable** and **whitespace-normalized**.
* No numeric indices required anywhere.
* Every token is an **Atom**.

### 2.2 The “Semantic 8 keywords” (your 8-tuple, keyboard-friendly)

Use these instead of symbols like `Q Σ L R δ s t r`:

1. **ACCEPT**
2. **REJECT**
3. **LEFT**
4. **RIGHT**
5. **START**
6. **STATE**
7. **ALPHABET**
8. **DELTA**

These map cleanly to your existing canvases (`accept.canvas`, `delta.canvas`, etc.) and remain mnemonic + embedding-friendly.

### 2.3 Relation-name tokens (R0..R7 without numbers)

Instead of `r0..r7`, use names:

* `NOTICE`  (observed facts)
* `ADJ`     (touching / neighbor)
* `CONTAIN` (inside / has-part)
* `ORIENT`  (direction / pose / frame)
* `ALIGN`   (consistent orientation / match)
* `TRACE`   (event edges)
* `CANON`   (canonicalization lens; derived)
* `META`    (meta-lens; derived)

Rule: `CANON` and `META` are **derived lenses** unless explicitly pinned as artifacts (if pinned, they must be reproducible from base relations).

### 2.4 Traversal tokens

These are *graph navigation primitives*:

* `AT(x)`
* `STEP(rel)`
* `FOLLOW(rel)` (multi-step closure)
* `SWITCH(line)` (change Fano line)
* `MEET(relA, relB)` (intersection / meet)
* `JOIN(relA, relB)` (union / join)
* `PROJECT(target)` (projection lens)
* `BARRIER(name)` (must satisfy invariant)
* `EMIT(event)` (append to TRACE)

### 2.5 Fold / Fano tokens

Keep Fano explicit but tokenized:

* Points: `P_A P_B P_C P_D P_E P_F P_G`
* Lines:  `L_ABC L_ADE ...` (use your normative table names)
* Generic: `POINT(x)` `LINE(x)` `INCIDENT(point,line)`

---

## 3) Binary Projection Spec (hash-based, reversible)

This is your **CAN-ISA / CLBC-POLY compatible** “shadow encoding” layer — **derived**, never authoritative.

### 3.1 Canonical source

Binary projection input is a **canonical UTF-8 serialization** of an IR node:

Canonical serialization rules:

* UTF-8 NFC normalization
* Keys sorted lexicographically
* No insignificant whitespace
* Arrays keep order
* Newlines normalized to `\n`

### 3.2 Symbol IDs (no fixed numeric opcodes)

Define **SymbolID** as a truncated hash of the canonical token:

* `SymbolID = TRUNC16( HASH256( "TETRA|" + token_utf8 ) )`
* Where `TRUNC16` yields 16 bits (or 24/32 later)

This gives:

* stable IDs
* deterministic across machines
* not hand-assigned numbers

### 3.3 Instruction envelope (reversible)

Define the binary instruction as:

* `tag` = `SymbolID(opcode_name)`
* `arg_count` = small varint
* `args[]` = each arg encoded as either:

  * `SYM(SymbolID(atom_token))`
  * `BYTES(blob)` for payloads (rare; must be hashed + referenced, not inlined for big data)

Encoding (conceptual):

```
[ MAGIC 'CANB' ]
[ VERSION ]
[ INSTR: tag16 | argc | arg1 | arg2 | ... ]
```

### 3.4 Decoding (reversal requirement)

A decoder must be able to produce:

* the `tag16` values
* plus a **symbol table** mapping `tag16 → token_utf8` bundled with the artifact (or derivable via registry file checked into repo)

**Rule:** If a `tag16` is unknown, decode must still preserve it as an opaque atom: `UNK(tag16)`.

### 3.5 Collision policy

Because truncation can collide:

* include optional `tag32` mode
* or include `(tag16, hash256_prefix64)` for disambiguation

**Canonical safety:** collisions never change meaning; they only require disambiguation.

---

## 4) CanvasL front-matter rules enforcing “no numbers stored”

This is the policy that makes your repo “pre-metric” and keeps numbers as **queries**, not **facts**.

### 4.1 File contract

Each `.canvasl` file is a **register**:

* It stores **atoms** and **relations**
* It may store **symbolic constraints**
* It must not store numeric measurements as truths

### 4.2 Allowed front matter schema (YAML)

Example:

```yaml
---
canvasl: "v1"
role: "register"
register: "STATE"          # one of the 8 keywords
relations:
  - NOTICE
  - ADJ
  - CONTAIN
  - ORIENT
  - ALIGN
  - TRACE
invariants:
  - FANO_CONSISTENT
  - CANON_IDEMPOTENT
derived_lenses:
  - CANON
  - META
no_numeric_facts: true
---
```

### 4.3 “No numeric facts” rule (mechanical)

Inside content:

* ✅ allowed: `clock: "FAST"`, `clock: "SLOW"`, `clock: "STABLE"`
* ✅ allowed: `ORDERED_BEFORE(A,B)`
* ✅ allowed: `CAPABLE_OF(device, opcode)`
* ❌ forbidden: `clock_hz: 240000000`
* ❌ forbidden: `ram_bytes: 524288`

If you need numbers:

* they must appear only as **derived outputs** in `TRACE` events (ephemeral)
* never as stable register facts

### 4.4 Query surface (numbers as views)

You can expose numeric views as queries:

* `COUNT(ADJ, node=X)`
* `MEASURE(TIME_RD)`
* `SIZE(TRACE.window)`

But those results are:

* ephemeral
* discardable
* never merged as facts

---

## 5) Lean statement: “binary projection preserves relational semantics”

Below is a **Lean-friendly skeleton** you can paste into `Formal/` later. It stays faithful to your philosophy: theorems about **relations**, not numbers.

### 5.1 Core definitions (sketch)

* `Atom` is an opaque type (tokens)
* `RelName` is an enum of relation names (`NOTICE`, `ADJ`, …)
* `State` is `RelName → Set (Atom × Atom)`

Define an opcode as a structure with `guard/effect/canon`.

### 5.2 Projection

Define:

* `encode : Program → ByteString`
* `decode : ByteString → Option Program` (or `ProgramWithUnknowns`)

Define semantic execution:

* `run : Program → State → State`

### 5.3 Theorems (what you actually want)

**Theorem A — Decode/encode roundtrip (up to α-equivalence)**

> Encoding then decoding yields a program that is semantically equivalent, even if symbol tags differ.

Lean shape:

* `SemEq : Program → Program → Prop`
* `SemEq p q := ∀ s, run p s = run q s`

Claim:

* `decode (encode p) = some p' → SemEq p p'`

**Theorem B — Binary is a refinement, not a source**

> Any decoded binary program corresponds to at least one canonical UTF-8 program that runs identically.

Claim:

* `decode b = some p → ∃ src, CanonicalUTF8 src ∧ parse src = p ∧ ...`

**Theorem C — Canon idempotence preserved**
If opcodes include canonicalization hooks:

* `∀ s, canon(canon(s)) = canon(s)`
  then after encoding/decoding this property still holds for the reconstructed opcodes (because canon is semantic, not numeric).

### 5.4 Why this matches your system

This lets you prove:

* the VM’s meaning lives in relations
* bytes are merely a transport shadow
* the compiler/decompiler pipeline is safe
