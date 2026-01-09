# Layer Algebra — Formal Collapse and Equivalence

**Status:** Specification (Draft)
**Layer:** −0 (Invariant)
**Relates to:** RFC-0001-TPPM
**Purpose:** Define collapse conditions and equivalence for multi-layer projections

---

## 1. Three Spaces (Typed Model)

### 1.1 Types

```
H : Hardware bitmap (bytes)
S : Software string / tokens (alphanumeric semantics)
V : View string (non-alphanumeric operators only)
```

### 1.2 Functions

```
anchor  : File → H           (stat→bitmap; identity-ish)
meaning : File → S           (content; names; code; words)
project : (H, V) → Region    (open/close regions, grouping, nesting)
inject  : (Region, S) → Doc  (place meanings into regions)
```

### 1.3 Trigonometric Analogy

```
H ≈ origin        (anchor; stable reference point)
S ≈ sin/cos       (values, content, meaning)
V ≈ tan           (boundary operator; angle of observation)
```

**Interpretation:**
- Software is **form** (ball)
- Meta-tags are **projection** (group two lines as a ball)
- Syntax/View is **sphere** (surface observation)

---

## 2. Layer Model (rₙ)

### 2.1 Definition

```
rₙ = degree of projection
```

Where:
- `n` = number of operators, nested levels, or repetitions
- `r₀` = minimal projection (single separator)
- `r₈` = maximal projection (octonion closure)

### 2.2 View String

A view string is a sequence over non-alphanumeric alphabet `Σᵥ`:

```
Σᵥ = { emoji, punctuation, whitespace } ∩ [^[:alnum:]]
```

---

## 3. Normalization

### 3.1 Normalization Function

```
NormV(v) :=
  1. Remove all alphanumeric characters (should already be absent)
  2. Collapse repeated whitespace to one space
  3. Map "separator family" to canonical token (optional)
```

### 3.2 Canonical Families

| Family | Canonical | Members |
|--------|-----------|---------|
| Whitespace | `␠` | space, tab, newline |
| Separator | `·` | `_`, `-`, `:`, `/`, `.`, `|` |
| Emoji | preserve | all emoji (preserve repetition for polynomial order) |

**Example:**

```
Input:  💻📝📝📝____term____
NormV:  💻📝📝📝·term·
```

---

## 4. Collapse Conditions

### 4.1 Partition Function

```
Partitionₙ(v) = set of open/close intervals and nesting relations
                obtained by parsing v up to depth n
```

### 4.2 Equivalence at Degree n

Two views are equivalent at degree `n` if they induce the same region partitioning:

```
v₁ ≡ₙ v₂  ⇔  Partitionₙ(NormV(v₁)) = Partitionₙ(NormV(v₂))
```

**Intuition:** Same sphere condition — same projection geometry, regardless of surface glyph choices.

### 4.3 Examples

```
💻📝📝📝  ≡₁  💻___     (same at n=1: one region)
💻📝📝📝  ≢₃  💻___     (different at n=3: nesting/degree differs)
```

**Rule:** Longer emoji strings = higher-degree operators = deeper context addressability

---

## 5. Wrapper Equivalence

### 5.1 Bracket Algebra

A **wrapper** is a pair `(L, R)` where:

```
L, R ∈ Σᵥ*   (sequences over non-alphanumeric alphabet)
L contains no alphanumerics
R contains no alphanumerics
```

A **term** is alphanumeric payload:

```
t ∈ S  (semantic content)
```

A **wrapped atom** is:

```
L · t · R
```

### 5.2 Wrapper Properties

```
Class(L) = canonical ID of L (after family-mapping)
Deg(L)   = length / polynomial order (or weighted length)
```

### 5.3 Wrapper Equivalence

```
(L₁,R₁) ≈ (L₂,R₂)  ⇔
    Class(L₁) = Class(L₂)  ∧
    Class(R₁) = Class(R₂)  ∧
    Deg(L₁)   = Deg(L₂)    ∧
    Deg(R₁)   = Deg(R₂)
```

**Relaxed form** (up to depth n):

```
(L₁,R₁) ≈ₙ (L₂,R₂)  ⇔  Partitionₙ(L₁·R₁) = Partitionₙ(L₂·R₂)
```

### 5.4 Examples (Structurally Equivalent)

```
[[term]]          # brackets
💻📝term📝💻      # emoji
<term>            # angle brackets
::term::          # colons
♤♡◇♧term♤♡◇♧    # card suits
```

All have the same structure:

```
P₁ · P₀ · P₁
```

---

## 6. Equivalence Modes

### 6.1 Strict Equivalence

```
v₁ ≡ v₂  ⇔  v₁ = v₂  (exact same operator sequence)
```

### 6.2 Loose Equivalence (at depth n)

```
v₁ ≡ₙ v₂  ⇔  Partitionₙ(v₁) = Partitionₙ(v₂)
```

### 6.3 Family Equivalence

```
v₁ ≡ₓ v₂  ⇔  Class(NormV(v₁)) = Class(NormV(v₂))
```

---

## 7. Why Layers Don't Bleed

### 7.1 Type Safety Constraints

```
H : Hardware    (MUST be byte-anchored)
S : Software    (MUST be alphanumeric)
V : View        (MUST be non-alphanumeric)
```

### 7.2 Enforcement Rules

1. **Software stays semantic** — `S ∩ Σᵥ = ∅`
2. **View stays non-semantic** — `V ∩ [:alnum:] = ∅`
3. **Hardware stays byte-anchored** — `H` independent of `S` and `V`

### 7.3 Resolution of Confusion

When layers bleed, equivalence becomes unprovable.

When layers are separated:
- Equivalence is **decidable**
- Collapse is **predictable**
- Projection is **composable**

---

## 8. Polynomial Order and Context Depth

### 8.1 Degree Mapping

```
Deg(v) = weighted length or repetition count

Examples:
Deg(💻)      = 1   (r₁)
Deg(💻📝)    = 2   (r₂)
Deg(💻📝📝📝) = 4   (r₄)
```

### 8.2 Context Addressing

Different lengths target different layers:

```
r₀ … r₈  (octonion closure)

r₀ = identity
r₁ = include (transition count)
r₂ = ignore (stable runs)
r₃ = schema (opens)
r₄ = scheme (closes)
r₅ = context (quotes)
r₆ = boundary (separators)
r₇ = collapse (newlines)
```

---

## 9. Normative Requirements

### 9.1 MUST

- Implementations MUST enforce type separation (H, S, V)
- Implementations MUST define `Partitionₙ` for their context depth
- Implementations MUST preserve polynomial order in view strings

### 9.2 SHOULD

- Implementations SHOULD define canonical families for `NormV`
- Implementations SHOULD expose equivalence mode (strict/loose/family)

### 9.3 MAY

- Implementations MAY define custom degree functions
- Implementations MAY extend beyond r₈ if needed

---

## 10. Reference Implementation

### 10.1 Normalization (POSIX sh)

```sh
normalize_view() {
  # Remove alphanumeric (should already be absent)
  # Collapse whitespace
  # Map separator families (optional)
  sed -E 's/[[:alnum:]]//g' | tr -s '[:space:]' ' '
}
```

### 10.2 Class Extraction

```sh
view_class() {
  normalize_view | sed 's/[_ \-:\/\.\|]/·/g'
}
```

### 10.3 Degree Calculation

```sh
view_degree() {
  awk '{ print length($0) }'
}
```

---

## 11. Summary

This specification defines:

1. **Three spaces** (H, S, V) with clear boundaries
2. **Collapse conditions** via `Partitionₙ` equivalence
3. **Wrapper equivalence** for emoji/separator strings
4. **Polynomial order** for context depth (r₀…r₈)

**Result:** Provable equivalence, no layer bleeding, clean composition.

---

## 12. References

- RFC-0001 TPPM (Two-Primitive Projection Model)
- GENESIS.org (Canonical declaration)
- genesis.sh (Reference implementation)
