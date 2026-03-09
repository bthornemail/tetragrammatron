# GENESIS Protocol - Reference Implementation

This document describes the POSIX reference implementation of RFC-0002-GENESIS-PROTOCOL.

## Quick Start

```bash
# Initialize a GENESIS repository
./genesis-protocol.sh init

# Run the complete workflow
./genesis-protocol.sh workflow

# Check status
./genesis-protocol.sh status
```

## Implementation Files

| File | Purpose |
|------|---------|
| `genesis-protocol.sh` | RFC-0002 reference implementation (POSIX shell) |
| `genesis.sh` | Original TPPM implementation (compatible) |
| `genesis-protocol/genesis.org` | Literate multi-layer Scheme/C implementation |

## Workflow Commands

### 1. Initialize Repository

```bash
./genesis-protocol.sh init [ROOT]
```

Creates:
- `.genesis/` directory for observations and views
- `.genesisignore` — patterns for dark matter (excluded files)
- `.genesisinclude` — patterns for light matter (observed files)
- `.genesisschema` — schema constraints

### 2. Layer 0: Generate (Observe)

```bash
./genesis-protocol.sh generate [ROOT] [OUT.jsonl]
```

Observes filesystem and creates append-only observation log:
- Reads POSIX metadata only (`stat`)
- Does NOT read file contents
- Generates Atoms (alphanumeric identity)
- Outputs to `.genesis/observations.jsonl`

**Example observation record:**

```json
{
  "ts": "2025-01-21T12:00:00Z",
  "atom": "READMEORG",
  "path": "README.org",
  "stat": {
    "dev": 2049,
    "ino": 123456,
    "mode": 33188,
    "uid": 1000,
    "gid": 1000,
    "size": 2064,
    "atime": 1737460800,
    "mtime": 1737460800,
    "ctime": 1737460800
  }
}
```

### 3. Layer 1: Contemplate (Project)

```bash
./genesis-protocol.sh contemplate [IN.jsonl] [VIEWDIR]
```

Projects observations to human-readable `.org` files:
- Creates `.genesis/views/` directory
- One `.org` file per Atom
- Includes properties and metadata
- Views are replaceable (can be regenerated)

**Example view file (`.genesis/views/READMEORG.org`):**

```org
#+TITLE: READMEORG
#+GENESIS_ATOM: READMEORG
#+GENESIS_PATH: README.org
#+GENESIS_OBSERVED: 2025-01-21T12:00:00Z

* Observation

This atom was observed at =2025-01-21T12:00:00Z=

Original path: =README.org=

* Properties

:PROPERTIES:
:ATOM: READMEORG
:PATH: README.org
:OBSERVED: 2025-01-21T12:00:00Z
:END:
```

### 4. Layer 2: Interpret (Schema/Scheme)

```bash
./genesis-protocol.sh interpret [SCHEMA] [OBSERVATIONS]
```

Applies schema constraints to observations:
- Reads `.genesisschema`
- Validates realm settings
- Extensible for custom rules
- Does NOT mutate observations

### 5. Layer 3: Translate (Emit)

```bash
./genesis-protocol.sh translate ATOM [SCHEME] [VIEWDIR] [OUTDIR]
```

Emits artifacts from atoms:
- Reads view files
- Applies transformation scheme
- Outputs to `.genesis/artifacts/`
- Reproducible and idempotent

**Available schemes:**
- `identity` — Copy view as-is
- `markdown` — Convert .org to .md
- (extensible)

### 6. Layer 7: Validate (Reconcile)

```bash
./genesis-protocol.sh validate [ROOT] [OBSERVATIONS]
```

Validates bijection between observations and filesystem:
- Counts observed atoms
- Counts current light files
- Reports drift
- Does NOT rewrite history

### 7. Complete Workflow

```bash
./genesis-protocol.sh workflow [ROOT]
```

Runs complete pipeline:
1. Generate (Layer 0)
2. Contemplate (Layer 1)
3. Interpret (Layer 2)
4. Validate (Layer 7)

## File Classification

Files are classified using `.genesisignore` and `.genesisinclude`:

| Class | Name | Meaning |
|-------|------|---------|
| UU | Unknown-Unknown | Dark matter (ignored) |
| KK | Known-Known | Light matter (observed) |
| KU | Known-Unknown | Deferred (not yet classified) |

**Example `.genesisignore` (dark matter):**

```
.git/
.genesis/
node_modules/
*.o
*.so
*.exe
*.bin
*.png
*.jpg
```

**Example `.genesisinclude` (light matter):**

```
*.org
*.md
*.sh
*.c
*.h
*.json
*.txt
```

## Core Principles (RFC-0002)

1. **Identity is alphanumeric only**
   - `README.org` → Atom: `READMEORG`
   - Separators (`.`, `-`, `/`, etc.) are views, not identity

2. **Observation precedes interpretation**
   - Layer 0 observes without reading content
   - Higher layers interpret and emit

3. **Append-only observations**
   - Observations are never deleted
   - New observations are appended
   - History is immutable

4. **Layer 7 validates Layers 0–6**
   - GENESIS is inverted: validation at top
   - Ensures bijection with reality

5. **Deterministic operations**
   - Same input always produces same output
   - No hidden state
   - No wall-clock dependency (except timestamps)

## Environment Variables

```bash
export GENESIS_DB=".genesis/observations.jsonl"
export GENESIS_IGNORE=".genesisignore"
export GENESIS_INCLUDE=".genesisinclude"
export GENESIS_SCHEMA=".genesisschema"
```

## Example Session

```bash
# Initialize repository
./genesis-protocol.sh init

# Observe current state
./genesis-protocol.sh generate

# Project to views
./genesis-protocol.sh contemplate

# Validate
./genesis-protocol.sh validate

# Check status
./genesis-protocol.sh status

# Translate specific atom
./genesis-protocol.sh translate READMEORG markdown
```

## Integration with Existing Tools

### With genesis.sh (TPPM implementation)

```bash
# Use genesis.sh for detailed fingerprinting
./genesis.sh translate README.org

# Use genesis-protocol.sh for workflow
./genesis-protocol.sh workflow
```

### With genesis-protocol/genesis.org (Literate implementation)

```bash
cd genesis-protocol/

# Tangle Scheme/C implementation
./genesis.sh

# Use POSIX workflow for observations
cd ..
./genesis-protocol.sh generate genesis-protocol/
```

## Extending the Implementation

### Add a new scheme for Layer 3

Edit `genesis-protocol.sh`, find `cmd_translate()`, add case:

```sh
case "$scheme" in
    # ... existing schemes ...
    json)
        # Custom JSON export scheme
        atom="$1"
        # ... your implementation ...
        ;;
esac
```

### Add custom schema validation

Edit `genesis-protocol.sh`, find `cmd_interpret()`, add rules:

```sh
# Custom validation logic
while IFS= read -r line; do
    # Parse observation
    # Apply custom rules
    # Report violations
done <"$observations"
```

## Compliance

This implementation conforms to:
- RFC-0002-GENESIS-PROTOCOL
- RFC-0001-TPPM (Two-Primitive Projection Model)
- POSIX.1-2008 shell scripting standard

Dependencies:
- POSIX shell (`sh`)
- Core utilities: `find`, `stat`, `sed`, `awk`, `wc`, `date`
- Optional: `jq` for advanced JSON manipulation

## See Also

- [RFC-0002-GENESIS-PROTOCOL](RFC-0002-GENESIS-PROTOCOL.md) — Full protocol specification
- [RFC-0001-TPPM](RFC-0001-TPPM.md) — Two-Primitive Projection Model
- [RFC-GENESIS-GLOSSARY-0001](RFC-GENESIS-GLOSSARY-0001.md) — Glossary of terms
- [CLAUDE.md](../CLAUDE.md) — Guide for working with this codebase
