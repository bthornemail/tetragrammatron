RFC-GENESIS-GLOSSARY-0001

Genesis Protocol — Glossary of Terms

**Status:** Draft
**Scope:** Applies to RFC-0001-TPPM (Two-Primitive Projection Model) and all Genesis-derived systems
**See also:**
- [RFC-0001-TPPM](RFC-0001-TPPM.md) — Two-Primitive Projection Model
- [RFC-0002-GENESIS-PROTOCOL](RFC-0002-GENESIS-PROTOCOL.md) — Full GENESIS Protocol

---

## A

### Atom

The smallest unit recognized by the Genesis Protocol.

An atom corresponds to a filesystem object that can be observed via `stat(2)`.

An atom:

- MUST exist at Layer −0 / 0
- MUST have a provenance derived from filesystem metadata
- MUST NOT require content inspection

---

### Authority

The level at which a piece of information is considered binding.

In Genesis:

- Filesystem metadata is authoritative
- Syntax is non-authoritative
- Views are non-authoritative
- Interpretation is optional and reversible

---

## B

### Block

A syntactic container introduced at Layer 2.

Examples:

- Org `#+BEGIN_SRC`
- Markdown code fences
- Emoji-wrapped spans

Blocks:

- MUST NOT affect identity
- MAY affect emission
- MUST be ignorable without breaking Genesis validity

---

### Bootstrap

The act of initializing a Genesis-compliant repository or directory.

Bootstrap:

- MUST begin at Layer −0 / 0
- MUST NOT assume syntax, language, or tooling
- MAY be implemented as a shell script (`genesis.sh init`)

---

## C

### Canonical

A representation that is authoritative within a given layer.

Canonical forms:

- Are deterministic
- Are minimal
- Are independent of presentation

Example:

- `st_ino` is canonical
- filename separators are not

---

### Collapse

The act of reducing multiple representations to a single authoritative identity.

Occurs at:

- GENESIS_LAYER 7

Collapse:

- MUST be idempotent
- MUST be explainable via lower layers
- MAY discard higher-layer distinctions

---

### Content

The bytes stored in a file.

Content:

- MUST NOT be read at Layer −0 / 0
- MAY be read at Layer 2+
- MUST NOT affect identity

---

## D

### Deterministic

Given the same inputs, the output is always identical.

Genesis operations:

- MUST be deterministic
- MUST avoid hidden state
- MUST not depend on wall-clock time (except where explicitly modeled)

---

## E

### Emission

The act of producing artifacts from syntax.

Occurs at:

- GENESIS_LAYER 3

Examples:

- Tangling code
- Exporting documents
- Producing buffers for rendering or execution

Emission:

- MUST NOT affect identity
- MAY be lossy
- SHOULD be reproducible

---

### Existence

The condition of being observable via the filesystem.

In Genesis:

- Existence is implied by successful `stat`
- No explicit boolean flag is required
- Non-existence is represented by failure to observe

---

## F

### File Descriptor

A POSIX reference to an open file.

In Genesis:

- File descriptors MAY be used as runtime projections
- File descriptors MUST NOT be confused with identity
- Identity precedes descriptors

---

### Footprint

A non-cryptographic provenance marker.

Derived from:

- inode
- device
- timestamps
- size

Footprints:

- Replace the need for hashes in Genesis
- Encode when and where something exists, not what it contains

---

## G

### GENESIS

The canonical authority file and semantic specification.

- `GENESIS.*` → authoritative definition
- `genesis.*` → operational implementation

**Uppercase defines what**
**Lowercase defines how**

---

### GENESIS_LAYER

A numbered stage in the Genesis pipeline.

Lower layers:

- Define identity and existence

Higher layers:

- Define syntax, emission, interpretation

All higher layers MUST collapse to lower layers.

---

### Genesis Protocol

A layered protocol for freezing identity before meaning.

The Genesis Protocol:

- Precedes syntax
- Precedes execution
- Precedes interpretation
- Enables consensus without agreement

---

## H

### Hash (Cryptographic)

A content-derived digest (e.g. SHA-256).

In Genesis:

- Cryptographic hashes are OPTIONAL
- MUST NOT be required for identity
- MAY be used in higher layers for integrity

Genesis prefers provenance chains over hashes.

---

## I

### Identity

The minimal invariant that answers the question:

> "Does this thing exist, and is it the same thing?"

In Genesis, identity is derived from:

- inode
- device
- ownership
- timestamps
- size

Identity:

- MUST be content-independent
- MUST be stable under renaming
- MUST be preserved across views

---

### Ignore / Include

Constraints applied during traversal.

- `.genesisignore` → exclusion contract
- `.genesisinclude` → inclusion contract

Rules:

- Ignored atoms are unknown-unknowns
- Included atoms MUST be processed
- Ignore/include MUST be evaluated before syntax

---

## L

### Layer −0 / 0

The existence layer.

Defines:

- Identity
- Provenance
- Authority

Disallows:

- Syntax
- Language
- Interpretation
- Execution

---

### Layer 7

The judgement layer.

Defines:

- Deduplication
- Collapse
- Validation

Ensures:

- Bijectivity with repository state
- Soundness of projections

---

## M

### Metadata

Filesystem-level information retrievable without reading content.

Examples:

- inode
- timestamps
- ownership
- permissions

Metadata is authoritative in Genesis.

---

## P

### Path

A string representation of a filesystem location.

In Genesis:

- `path` is a view
- `PATH` is a virtual template
- Neither is authoritative
- Both may change without affecting identity

---

### Polynomial View

A grouping or ordering of terms using non-alphanumeric separators.

Examples:

```
A9F3_C21D
A9F3:C21D
♤♡◇♧A9F3C21D♧◇♡♤
```

Polynomial views:

- MUST be reversible
- MUST NOT affect identity

---

## R

### Reference Implementation

A minimal, portable implementation used to demonstrate conformance.

For Genesis:

- POSIX shell is RECOMMENDED
- `stat`, `find`, `awk`, `sed` are sufficient
- Higher-level languages are optional

---

### Repository

A directory tree under Genesis governance.

A Genesis-compliant repository:

- Is append-only at identity level
- Supports projection into multiple views
- Preserves provenance across transformations

---

## S

### Schema

A set of constraints applied after identity.

Schemas:

- MUST NOT redefine identity
- MAY constrain interpretation
- MAY coexist in parallel

---

### Separator

Any non-alphanumeric character.

Separators:

- Are non-authoritative
- Are grouping operators only
- MAY be emoji
- MUST be ignorable

---

## V

### View

A projection of identity for human or machine use.

Examples:

- filenames
- casing
- syntax
- markup
- emojis
- renderings

Views:

- MUST NOT affect identity
- MUST be replaceable
- MAY be layered arbitrarily

---

## Z

### Zero Polynomial

The canonical "no-information" reference.

Often represented as:

```
00:00:00:00
```

Used to:

- Anchor hardware constraints
- Define neutral projections
- Establish reference origins

---

## Final Note (Non-Normative)

This glossary exists to ensure that when two implementations disagree, they disagree about views, not existence.

That is the entire purpose of Genesis.
