RFC-0001
Two-Primitive Projection Model (TPPM)

**Status:** Standards Track (Draft)
**Layer:** −0 (Invariant / Pre-Semantic)
**Updates:** None
**Obsoletes:** None
**Intended Audience:** Humans, tools, interpreters, compilers, indexers

---

## 1. Abstract

This document defines a universal minimal model for text, structure, and meaning separation based on exactly two primitive groups and a single compositional rule.
The model is designed to operate prior to syntax, semantics, execution, or belief, and to be compatible with POSIX systems, symbolic computation, and human reasoning.

---

## 2. Conformance Language

The key words MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT, MAY, and OPTIONAL in this document are to be interpreted as described in RFC 2119.

---

## 3. Primitive Groups (Normative)

An implementation MUST recognize exactly two and only two primitive groups.

### 3.1 Primitive P₀ — Semantic Mass

```
P₀ := [[:alnum:]]+
```

P₀ represents semantic content.

P₀ MUST include:
- alphabetic characters
- numeric characters

P₀ MUST NOT encode:
- boundaries
- grouping
- context
- projection

---

### 3.2 Primitive P₁ — Structural Projection

```
P₁ := [^[:alnum:]]+
```

P₁ represents structure without meaning.

P₁ MUST include:
- punctuation
- whitespace
- symbols
- emoji
- markup
- delimiters

P₁ MUST NOT encode semantic identity.

---

### 3.3 Exhaustiveness Rule

Every byte in an input stream MUST belong to exactly one primitive group.

There MUST NOT exist a third primitive.

---

## 4. Fundamental Composition Rule (Normative)

The only valid composition operation is alternation.

All valid expressions MUST reduce to one of the following canonical forms:

```
(P₀ ∘ P₁)* ∘ P₀?
(P₁ ∘ P₀)* ∘ P₁?
```

No other structural form is permitted at Layer −0.

---

## 5. Order and Magnitude (Normative)

Primitive type does not encode meaning.

Meaning MAY be inferred from order.

### 5.1 Run Length

The order of a primitive is defined as:

```
order(x) = length of contiguous run
```

Examples:

| Input      | Projection | Polynomial |
|------------|------------|------------|
| `a`        | P₀         | P₀¹        |
| `foo`      | P₀         | P₀³        |
| `:`        | P₁         | P₁¹        |
| `💻📝📝`   | P₁         | P₁³        |

Order MAY be interpreted as dimensional magnitude, emphasis, or layer depth.

---

## 6. Shape Is Derived (Non-Primitive)

The following are derived interpretations and MUST NOT be treated as primitives:

| Derived Shape | Structural Pattern |
|---------------|--------------------|
| Point         | P₀ or P₁           |
| Line          | P₀ ∘ P₁            |
| Plane         | P₀ ∘ P₁ ∘ P₀       |
| Ball          | P₁ ∘ P₀ ∘ P₁       |
| Sphere        | max(P₁ run length) |

Shapes are views, not identities.

---

## 7. Equivalence (Normative)

Two expressions are equivalent iff:

1. They collapse to the same primitive alternation sequence
2. Their run-length vectors are equal after projection

### 7.1 Collapse Function (Reference)

```sh
sed -E 's/[[:alnum:]]+/P0/g; s/[^[:alnum:]]+/P1/g'
```

---

## 8. Hardware / Software Duality (Normative)

### 8.1 Hardware Constraint

A hardware identity MUST be representable as a fixed-width hexadecimal bitmap:

```
^(?:[0-9A-Fa-f]{2}:)+[0-9A-Fa-f]{2}$
```

This represents addressability, not meaning.

---

### 8.2 Software Form

Software form MUST preserve alphanumeric semantics.

Software form MUST NOT be removed during projection.

---

### 8.3 Syntax / View

Syntax MUST be expressed exclusively through P₁.

Syntax SHALL NOT define identity.

---

## 9. Meta-Tags (Normative)

A meta-tag is defined structurally, not symbolically.

A valid meta-tag MUST conform to:

```
P₁ ∘ P₀ ∘ P₁
```

Examples (structurally equivalent):

```
[[ term ]]
#+BEGIN_SRC sh
💻📝 term 📝💻
<term>
```

All are equivalent at Layer −0.

---

## 10. Stability Principle (Informative)

> Only what responds can remain.
> Only what alternates can be understood.

This model is:
- encoding-agnostic
- language-agnostic
- metaphysics-safe
- POSIX-executable

---

## 11. Security Considerations

This specification introduces no executable semantics and therefore introduces no security vulnerabilities at Layer −0.

---

## 12. IANA Considerations

None.

---

## 13. Conclusion

This RFC defines definition itself:
- prior to syntax
- prior to meaning
- prior to execution
- prior to belief

All higher layers MAY be built on top of it.
None MAY violate it.

---

## 14. References

- **RFC-0002-GENESIS-PROTOCOL**: Full GENESIS Protocol specification
- **RFC-GENESIS-GLOSSARY-0001**: Genesis Protocol — Glossary of Terms
