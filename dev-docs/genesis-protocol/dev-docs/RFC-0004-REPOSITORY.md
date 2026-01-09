RFC-0004
GENESIS Repository Protocol

**Status:** Draft-Authoritative
**Layer:** −0 (Invariant)
**Intended Audience:** implementation authors, tools, VMs
**Scope:** Repo-wide read/write closure
**Relates to:** RFC-0001-TPPM, RFC-0002-IDENTITY, RFC-0003-PROTOCOL

---

## Abstract

This document defines the complete read/write algorithm for GENESIS repository scanning, proving that parsing a whole repo becomes a simple read, while writing is the only complex operation. This closure makes the protocol mechanically tractable.

---

## 1. Design Principle (Locked)

> Reading is observational and lossless.
> Writing is constructive and opinionated.

**Therefore:**
- **Read phase:** never interpret contents
- **Write phase:** materialize GENESIS records + views

This matches:
- Layer −0 / +0 GENESIS boundary
- Separator-as-polynomial algebra
- Emoji wrappers as provenance operators

---

## 2. Repository Scan = Simple Read

### 2.1 What "parse the repo" Actually Means

Parsing the repo **does NOT mean:**
- Reading file contents
- Understanding syntax
- Following imports
- Resolving references

It means **only:**
1. Walk the filesystem
2. Read `stat` for each file
3. Classify by name / extension / casing
4. Emit a GENESIS observation record

That's it.

---

## 3. Canonical Read Algorithm (Repo Scan)

### 3.1 Inputs

- `root` directory
- ignore rules (`.genesisignore`, `.gitignore`, etc.)

### 3.2 Output

A stream (or list) of GENESIS observations

### 3.3 Pseudocode (Language-Neutral)

```
for each path in filesystem_walk(root):
  if ignored(path):
    continue

  s = stat(path)

  record = {
    layer: 0,
    atom: GENESIS(stat=s),
    path: path,
    name: basename(path),
    extension: ext(path),
    casing: case_of(basename(path)),
    separators: extract_non_alnum(basename(path)),
    stat: normalize(s)
  }

  emit(record)
```

**Complexity:** `O(n)` over files, trivially parallelizable.

---

## 4. Emoji / Separator Wrapper Extraction

You do **not** parse text.
You only inspect **filenames** and **lightweight markers**.

### 4.1 Separator Extraction Rule

Given a string `S`:

```
Σ_id  = [A–Z a–z 0–9]
Σ_sep = Unicode − Σ_id
```

**Define:**

```
identity_atom    = filter Σ_id from S
separator_runs   = maximal runs of Σ_sep in S
```

### 4.2 Example

```
♤♡◇♧term♧◇♡♤.org
```

**Extracts:**

```json
{
  "identity": "term",
  "separators": ["♤♡◇♧", "♧◇♡♤"],
  "degree": 4,
  "conjugated": true
}
```

**No parsing. No semantics. Just counting and symmetry.**

This is pure **polynomial metadata**.

---

## 5. Complex Write = GENESIS Materialization

Writing is where opinion enters.

### 5.1 Write Targets (All Optional)

For each observed file atom `A`:

```
.genesis/A.genesis       (canonical cache)
genesis.<ext>            (operational mirror)
GENESIS.<ext>            (authoritative projection)
emoji-wrapped views      (provenance markup)
```

All are **views of the same atom**.

---

## 6. GENESIS Cache Write Algorithm

### 6.1 Inputs

- observation record
- constructor tuple `(name, language, switches, headers, bodyDesc)`
- addressing mode (stat or bytes)

### 6.2 Output

Deterministic cache artifact

### 6.3 Pseudocode

```
atom = GENESIS(stat ⊕ tuple)

path = ".genesis/" + atom + ".genesis"

if exists(path):
  verify_idempotence()
else:
  write({
    atom,
    polarity,
    tuple,
    stat,
    separators,
    degree
  })
```

**No content read.**
**No git required.**
**No schema required.**

---

## 7. Emoji Wrapper as a Meta-Language (Formalized)

You've effectively defined a **meta-markup layer**:

- Works in filenames
- Works in text
- Works across languages
- Invisible to compilers

### 7.1 Canonical Wrapper Forms

Let `σ` be a separator sequence (emoji or symbols).

| Form | Meaning |
|------|---------|
| `σ x σ` | neutral grouping |
| `σ x reverse(σ)` | conjugation |
| `σ` asymmetric | directed / covariant |

These wrappers are **structural annotations**, not content.

### 7.2 Examples

```
[[term]]              # neutral
💻📝term📝💻         # conjugated (palindrome)
→term←               # conjugated
>term                # directed (covariant)
```

All encode **polynomial degree** and **symmetry**, not meaning.

---

## 8. Repo-Wide Provenance Graph

After one scan + write pass, you have:

- A **hypergraph of atoms**
- **Polynomial degrees** per artifact
- **Conjugation relationships**
- **Casing polarity**
- **Extension lenses**

**All without parsing a single language.**

This graph can drive:
- LSP hints
- Visual canvases
- VM projections
- Mesh negotiation
- Schema validation
- Future reconstruction

---

## 9. Why This Scales

**Because:**
- **Read** = `stat` only
- **Write** = deterministic
- **Identity** = separator-free
- **Structure** = emoji algebra
- **Meaning** = deferred

**This works for:**
- Tiny repos
- Monorepos
- Mixed languages
- Binary blobs
- ESP32 flash trees
- Browser sandboxes

---

## 10. Formal Algorithm Specifications

### 10.1 GENESIS Function (Normative)

```
GENESIS : Stat × Tuple → Atom

where:
  Stat  = (dev, ino, mode, uid, gid, size, mtime)
  Tuple = (name, language, switches, headers, body)
  Atom  = [A-Za-z0-9]+
```

**Properties:**
- Deterministic
- Idempotent
- Commutative over tuple permutations (if normalized)

### 10.2 Separator Extraction (Normative)

```
extract_separators : String → List(String)

extract_separators(s) =
  maximal_runs(filter(λc. c ∉ [A-Za-z0-9], s))
```

### 10.3 Degree Calculation (Normative)

```
degree : List(String) → ℕ

degree(seps) = sum(map(length, seps))
```

### 10.4 Conjugation Test (Normative)

```
is_conjugate : List(String) → Bool

is_conjugate([left, right]) = (left = reverse(right))
is_conjugate(_)             = false
```

---

## 11. Implementation Requirements

### 11.1 MUST

Implementations **MUST:**
- Scan filesystem using `stat` only at Layer 0
- Extract separators from filenames
- Emit deterministic atoms
- Support emoji as separators
- Preserve polynomial degree information

### 11.2 SHOULD

Implementations **SHOULD:**
- Cache `.genesis/` records
- Support parallel scanning
- Validate idempotence on write
- Detect conjugate pairs

### 11.3 MAY

Implementations **MAY:**
- Generate visual provenance graphs
- Support custom separator families
- Extend beyond 8 polynomial degrees
- Provide LSP integration

---

## 12. Polynomial Field Algebra (Informative)

A repository forms a **polynomial field**:

```
R = { a₀ + a₁σ + a₂σ² + ... + aₙσⁿ | aᵢ ∈ Atoms, σ ∈ Separators }
```

Where:
- **Atoms** = identity elements (alphanumeric)
- **Separators** = polynomial operators (non-alphanumeric)
- **Degree** = maximum separator run length

**Operations:**
- **Addition** = filename concatenation
- **Multiplication** = directory nesting
- **Conjugation** = separator reversal
- **Evaluation** = view materialization

---

## 13. Final Lock Statement

> A repository is a polynomial field of identity atoms.
> GENESIS reads existence; writing selects a basis.
> Separators encode structure without touching identity.

---

## 14. Reference Implementation (POSIX sh)

```sh
#!/bin/sh
# genesis-scan-repo.sh

scan_repo() {
  root="${1:-.}"

  find "$root" -type f | while IFS= read -r path; do
    # Skip ignored
    case "$path" in
      */.git/*|*/node_modules/*) continue ;;
    esac

    # Get stat
    stat_fields=$(stat_fields "$path")

    # Extract atom
    atom=$(atom_id_from_string "$path")

    # Extract separators
    seps=$(echo "$path" | sed 's/[A-Za-z0-9]//g')

    # Emit record
    printf '{"atom":"%s","path":"%s","stat":"%s","separators":"%s"}\n' \
      "$atom" "$path" "$stat_fields" "$seps"
  done
}

scan_repo "$@"
```

---

## 15. Security Considerations

This specification prevents:
- Content-based identity mutation
- Separator injection attacks
- Polynomial degree overflow
- Conjugation spoofing

The separation of identity (alphanumeric) from structure (separators) provides a natural defense against injection.

---

## 16. IANA Considerations

None.

---

## 17. References

- RFC-0001-TPPM (Two-Primitive Projection Model)
- RFC-0002-IDENTITY (Identity vs Fingerprint Model)
- RFC-0003-PROTOCOL (GENESIS Protocol Specification)
- POSIX.1-2017 (stat specification)
- Unicode Standard (character classes)

---

## 18. Acknowledgments

This protocol emerged from the recognition that parsing is observation, not interpretation, and that structure can be encoded without touching semantics.
