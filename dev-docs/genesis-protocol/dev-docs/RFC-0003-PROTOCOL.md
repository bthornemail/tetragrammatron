RFC-0003
GENESIS Protocol Specification

**Status:** Draft-Authoritative
**Layer:** −0 (Invariant)
**Intended Audience:** humans, shell scripts, editors, VMs
**Scope:** repository-scale provenance, interpretation, and compilation
**Relates to:** RFC-0001-TPPM, RFC-0002-IDENTITY

---

## 1. Normative Language

The key words MUST, MUST NOT, SHOULD, SHOULD NOT, and MAY in this document are to be interpreted as described in RFC 2119.

---

## 2. Core Axiom (Lock This)

> Identity is alphanumeric only.
> Separators are views, not identity.

All semantics flow from this rule.

---

## 3. Canonical Objects

### 3.1 Atom

An **Atom** is the minimal identity unit.

**MUST:**
- Consist only of `[A–Z a–z 0–9]`
- Be derived from file provenance, not file content
- Be stable under renaming, separators, or casing changes

**Examples of the same Atom (different views):**

```
A9F3C21D
a9f3c21d
A9F3_C21D
A9F3:C21D
A9F3/C21D
A9F3.C21D
♤♡◇♧A9F3C21D♧◇♡♤
```

---

### 3.2 Separator (View Operator)

A **Separator** is any character sequence not containing `[A–Z a–z 0–9]`.

**MUST NOT:**
- Affect identity

**MAY be used to:**
- Group terms
- Encode views
- Provide human or symbolic structure

**Emoji sequences MAY be used as separators.**

Separators are **polynomial grouping operators**, not semantic operators.

---

## 4. GENESIS Layers (Frozen)

### Layer −0 / +0 (GENESIS_LAYER_0)

**Purpose:** Observe reality without interpretation.

**MUST:**
- Read POSIX file metadata only (stat, ls)
- **MUST NOT** read file contents
- Emit an append-only observation record
- Produce an Atom from: `(stat tuple + fixed-arity polynomial coordinates)`

This layer is **pure observation**.

---

### Layer 1 (ORG Projection)

**Purpose:** Human-readable projection.

**MAY:**
- Use .org, .md, .jsonl, etc.

**MUST:**
- **MUST NOT** redefine identity
- Reference Atoms via property drawers or headers

**MAY:**
- Be regenerated at any time

---

### Layer 2 (Schema / Scheme)

**Purpose:** Interpretation rules.

```
.genesisschema = declarative constraints
.genesisscheme = executable transformations
```

**MUST:**
- **MUST NOT** mutate Layer 0 records
- Be replaceable

---

### Layer 3 (Emitter)

**Purpose:** Materialization.

Emits:
- code
- binaries
- configs
- canvases
- views

**MUST:**
- Factor execution through prior layers
- Be idempotent

---

### Layer 7 (Judgement / Reconciliation)

**Purpose:** Verification and validation.

**MUST:**
- Ensure bijection between:
  - .git tree
  - GENESIS observations

**MAY:**
- Reject projections

**MUST NOT:**
- Rewrite history

This is the **commit / merge / reconcile gate**.

> GENESIS is inverted:
> Layer 7 validates Layers 0–6.

---

## 5. Canonical Files (Minimal Kernel)

These files form a complete block design.
They are isomorphic and idempotent.

| File | Role |
|------|------|
| **README.org** | What this is |
| **AGENTS.org** | Who/what may act |
| **MANIFESTO.org** | Why this exists |
| **GENESIS.org** | How generation works |
| **INDEX.org** | Optional explicit index |
| **IGNORE.org** | Dark / unknown paths |
| **INCLUDE.org** | Light / observed paths |

**Optional narrative extensions (non-authoritative):**
- CONVERSATION.org
- REVELATION.org
- SCHEMA.org
- SCHEME.org

---

## 6. GENESIS Block Syntax (Canonical)

Both uppercase and lowercase forms are valid.

```
GENESIS <name> <language> <switches> <header-args> <body>
genesis <name> <language> <switches> <header-args> <body>
```

All fields are **OPTIONAL**.

Missing fields **MUST** default to `"00"`.

This yields a **fixed-arity polynomial:**

```
[name | language | switches | headers | body]
```

**Example zero polynomial:**

```
00:00:00:00:00
```

---

## 7. Uppercase vs Lowercase Semantics

| Form | Meaning |
|------|---------|
| **GENESIS.org** | Semantic authority |
| **genesis.org** | Operational mirror |

This distinction:
- **MUST NOT** affect identity
- **MAY** affect tooling behavior
- Enables case-based instruction tracing

---

## 8. Addressing Model (IPv6 / BIP-style)

Addressing is **radix-based**:
- Fixed arity (e.g. 8×5 registers)
- Separators are visual only
- Canonical address is separator-free

**Example view:**

```
A9F3:C21D:8B02:11
```

**Canonical identity:**

```
A9F3C21D8B0211
```

---

## 9. Genesis Function (Abstract Algorithm)

### generate(blob)

**Input:** file stat observation
**Output:** Atom

**MUST be:**
- Deterministic
- Append-only

---

### contemplate(atom)

**Input:** Atom
**Output:** projections, views, schemas

**MUST NOT:**
- Alter provenance

---

### translate(atom, scheme)

**Input:** Atom + scheme
**Output:** emitted artifact

**MUST be:**
- Reproducible

---

## 10. Workflow Diagram (Authoritative)

```
┌────────────┐
│ Filesystem │
└─────┬──────┘
      │  stat (observe)
      ▼
┌──────────────┐
│ GENESIS L0   │  ← append-only
│ Observation  │
└─────┬────────┘
      │ project
      ▼
┌──────────────┐
│ ORG / Views  │  ← replaceable
└─────┬────────┘
      │ interpret
      ▼
┌──────────────┐
│ Schema/Scheme│
└─────┬────────┘
      │ emit
      ▼
┌──────────────┐
│ Artifacts    │
└─────┬────────┘
      │ validate
      ▼
┌──────────────┐
│ GENESIS L7   │  ← judgement
│ Reconcile    │
└──────────────┘
```

---

## 11. Glossary

### Atom
Canonical alphanumeric identity.

### Separator
Non-alphanumeric grouping symbol; view only.

### Observation
Metadata-only record (no content).

### Projection
Human-readable or tool-readable view.

### Schema
Constraint description.

### Scheme
Executable interpretation.

### Judgement
Validation of bijection and consistency.

### Light
Included, observed, named.

### Dark
Ignored, unknown, unobserved.

---

## 12. Implementation Guarantee

If an implementation follows this RFC:

✓ It can parse entire repositories with a simple read
✓ It can write complex systems deterministically
✓ It decouples observation from interpretation
✓ It scales from shell scripts to VMs
✓ It remains human-navigable

---

## 13. Final Lock-In Statement

> GENESIS is not a build system.
> It is a protocol for freezing meaning in time
> without freezing interpretation.

---

## 14. Security Considerations

This specification prevents:
- Identity mutation via content changes
- Observation contamination via views
- Provenance loss through layer collapse
- Interpretation authority confusion

---

## 15. IANA Considerations

None.

---

## 16. References

- RFC-0001-TPPM (Two-Primitive Projection Model)
- RFC-0002-IDENTITY (Identity vs Fingerprint Model)
- GENESIS.org (Canonical declaration)
- POSIX stat(2) specification
- RFC 2119 (Key words for RFCs)
