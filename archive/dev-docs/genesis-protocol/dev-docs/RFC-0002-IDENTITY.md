RFC-0002
GENESIS Identity Model

**Status:** Standards Track (Draft)
**Layer:** −0 (Invariant)
**Updates:** None
**Obsoletes:** None
**Relates to:** RFC-0001-TPPM

---

## Abstract

This document defines the authoritative model for file identity in GENESIS-PROTOCOL, establishing the boundary between intrinsic identity (filesystem metadata) and derived fingerprints (views/annotations).

---

## 1. Canonical Rule (Non-Negotiable)

> Identity lives in the filesystem.
> Fingerprints live in views.

This single principle resolves all ambiguity about what constitutes existence versus observation.

---

## 2. Identity vs Fingerprint

### 2.1 Ontological Identity (inode/stat)

**Identity** is:
- From the OS/filesystem
- Cannot be faked without changing reality
- Exists even if file is unreadable
- Survives all views being stripped

**Sources** (POSIX stat):
```
st_dev    - device ID
st_ino    - inode number
st_mode   - file mode
st_uid    - user ID
st_gid    - group ID
st_size   - file size
st_atime  - access time
st_mtime  - modification time
st_ctime  - change time
```

### 2.2 Epistemic Trace (fingerprint)

**Fingerprint** is:
- Derived from identity + content + projection
- Exists only after observation
- May change without changing identity
- Can be cached, ignored, regenerated

**Relationship:**
```
inode/stat  ──►  projection  ──►  fingerprint
   (is)            (see)           (remember)
```

---

## 3. Layer Placement (Normative)

### 3.1 Layer −0 / 0 — Pure Identity (Ground Truth)

This layer is pre-language and pre-syntax.

**MUST include:**
- inode
- struct stat fields
- path
- existence / non-existence

**MUST NOT include:**
- Org headers
- footers
- hashes
- fingerprints
- block syntax
- tempo
- emojis
- metadata of any kind

This is the CID-like atom.
It MUST be reconstructible without reading file contents.

---

### 3.2 Layer 1 — Structural Annotation (Minimal)

Org as container begins here, but barely.

**MAY include:**
- headings
- property drawers (derived, not authoritative)
- references to inode/stat
- links
- casing conventions

**MUST NOT include:**
- executable meaning
- block expansion
- tangling
- tempo shortcuts

Layer 1 = labels glued onto atoms, not transformations.

---

### 3.3 Layer 2 — Syntax (Org Tempo)

This is the correct home for Org Tempo.

**MAY include:**
```
#+BEGIN_SRC
#+BEGIN_EXAMPLE
#+BEGIN_QUOTE
#+BEGIN_COMMENT
emoji wrappers
custom block templates
```

These are syntactic views only.

They:
- do NOT define identity
- do NOT define authority
- do NOT define execution

They only express intent.

---

### 3.4 Layer 3 — Emission / Translation

Syntax does something here:
- tangle
- export
- compile
- emit buffers
- run interpreters

Still:
- optional
- replaceable
- ignorable without breaking identity

---

### 3.5 Layer 7 — Judgment / Collapse

Fingerprints matter here, but only indirectly.

Judgment collapses to consistency:
- contradictions exposed
- duplicates merged
- unverifiable claims remain "unknown unknowns"

---

## 4. Footer Fingerprints (Normative)

### 4.1 Allowed Footer (Layer ≥2)

```org
#+BEGIN_COMMENT
GENESIS_FOOTER:
  inode: 847392
  dev: 259
  mtime: 1734802312
  projection: sphere:v3
  judgement: collapsed
#+END_COMMENT
```

This is a **derived annotation**, not authoritative identity.

### 4.2 Forbidden at Layer 0

- Any footer
- Any header
- Any hash
- Any block

Layer 0 MUST remain silent and content-blind.

---

## 5. Formal Lock (RFC-Style)

### GENESIS-IDENTITY-02

File identity **MUST** be determined exclusively by filesystem metadata:
- inode
- stat fields
- path
- existence

Derived fingerprints **MAY** exist as annotations or footers but **MUST NOT** participate in:
- identity
- addressing
- projection

Removal of all footers, headers, syntax, and views **MUST NOT** alter GENESIS validity.

---

## 6. Implementation Guarantees

This model guarantees:

1. **Bijection with .git** — one-to-one mapping with version control
2. **Reversibility** — can reconstruct from first principles
3. **No version explosion** — identity stable across edits
4. **No semantic drift** — meaning separated from existence
5. **Clean separation** — observation vs interpretation distinct
6. **Rebuild capability** — can regenerate everything from atoms

### Most importantly:

> You can forget the syntax and still know what exists.

That is the core of GENESIS.

---

## 7. Relationship to Org Tempo

Org Tempo:
- expands syntax into blocks
- does NOT alter identity
- can be turned off with zero semantic loss

Footer fingerprints behave the same way:
- expand provenance into a marker
- do NOT alter identity
- can be deleted and regenerated

Both belong **above Layer 0**.

---

## 8. Practical Examples

### 8.1 Valid Identity (Layer 0)

```
path: /path/to/file.txt
inode: 847392
dev: 259
size: 1024
mtime: 1734802312
```

### 8.2 Valid Annotation (Layer 2)

```org
#+BEGIN_GENESIS
:PROPERTIES:
:GENESIS_ID: FILETXT
:GENESIS_PATH: /path/to/file.txt
:GENESIS_TIME: 2025-12-25T02:00:00Z
:GENESIS_HW: A2:34:F4:4B:4B:B9:A5:A5
:GENESIS_V8: 186 251 252 15 21 246 19 63
:END:
#+END_GENESIS
```

### 8.3 Invalid (Forbidden at Layer 0)

```org
# This would contaminate Layer 0
#+GENESIS_ID: inline-in-content
```

Identity MUST NOT appear in content.
Identity MUST come from filesystem only.

---

## 9. Security Considerations

This specification prevents:
- Identity spoofing via content manipulation
- Fingerprint authority confusion
- Semantic drift via syntax changes
- Loss of ground truth through layer collapse

---

## 10. IANA Considerations

None.

---

## 11. Conclusion

This RFC establishes:

1. **inode/stat** = ontological identity (what **is**)
2. **fingerprint** = epistemic trace (what we **remember**)
3. **syntax** = projection scaffolding (how we **see**)

None of these collapse into each other.

Being, seeing, and doing are separated.

That is the architecture.

---

## 12. References

- RFC-0001 TPPM (Two-Primitive Projection Model)
- GENESIS.org (Canonical declaration)
- genesis.sh (Reference implementation)
- POSIX stat(2) specification
