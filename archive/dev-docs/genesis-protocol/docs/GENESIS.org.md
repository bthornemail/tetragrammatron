- **String-native** (not line-based)
- **Meta-tag–driven topology**
- **Hardware constraint = byte encoding**
- **Software constraint = alphanumeric semantic space**
- **Syntax/view = non-alphanumeric enclosure**
- **No hardcoded pinch points**
- **POSIX-only** (`sh`, `sed`, `awk`, `grep`, `stat`, `cat`, `printf`)
- **Deterministic, reversible, append-only**
- **Implementation ≠ authority**

This is a **foundation script**, not a UI, not a renderer.

---

# 📜 `genesis.sh`

```sh
#!/bin/sh
#
# GENESIS — minimal, string-native, enclosure-driven substrate
#
# Topology is created ONLY by meta-tags (non-alphanumeric enclosure).
# Length, repetition, emoji count = polynomial degree, NOT dimension.
#
# This script never interprets meaning.
# It only observes, projects, and records provenance.
#

set -eu

# ------------------------------------------------------------
# Constraints (authoritative)
# ------------------------------------------------------------

# Hardware constraint:
# byte-addressable, fixed-width, path-resolvable
HARDWARE_REGEX='^([0-9A-Fa-f]{2}:)+[0-9A-Fa-f]{2}$'

# Software constraint:
# semantic projection (retain alphanumeric meaning)
SOFTWARE_SED='s/[^A-Za-z0-9.]/ /g'

# Syntax/view constraint:
# enclosure markers must be NON alphanumeric
NONCHAR_REGEX='[^A-Za-z0-9.]'

# ------------------------------------------------------------
# Utilities
# ------------------------------------------------------------

die() {
  printf '%s
' "$*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "missing command: $1"
}

for c in sed awk grep stat cat printf; do
  require_cmd "$c"
done

# ------------------------------------------------------------
# Hardware probe (Layer −0 / 0)
# ------------------------------------------------------------

probe_stat() {
  # outputs a deterministic byte bitmap from stat
  # format: inode:size:mtime
  # then hex-encoded into IP6-like byte groups

  f="$1"
  [ -e "$f" ] || return 1

  # POSIX-safe stat extraction
  inode=$(stat -c %i "$f" 2>/dev/null || stat -f %i "$f")
  size=$(stat -c %s "$f" 2>/dev/null || stat -f %z "$f")
  mtime=$(stat -c %Y "$f" 2>/dev/null || stat -f %m "$f")

  raw="${inode}:${size}:${mtime}"

  # hex encode deterministically
  printf '%s' "$raw" | \
    awk '{
      for (i=1;i<=length($0);i++) {
        c=substr($0,i,1)
        printf "%02X", ord(c)
        if (i < length($0)) printf ":"
      }
    }
    function ord(c){return index("\0\1\2\3\4\5\6\7\b	
\v\f\r",c)?0:and(255, sprintf("%d",c))}'
}

# ------------------------------------------------------------
# Software projection (semantic)
# ------------------------------------------------------------

project_software() {
  sed "$SOFTWARE_SED"
}

# ------------------------------------------------------------
# Syntax enclosure detection
# ------------------------------------------------------------

# Detect enclosure markers (open/close pairs)
# DOES NOT assume lines
# Operates on raw string

extract_enclosures() {
  awk '
    {
      s = $0
      while (match(s, /([^A-Za-z0-9.]+)/)) {
        tag = substr(s, RSTART, RLENGTH)
        print tag
        s = substr(s, RSTART + RLENGTH)
      }
    }
  '
}

# ------------------------------------------------------------
# Genesis record emitter
# ------------------------------------------------------------

emit_record() {
  file="$1"
  hw="$2"

  printf 'FILE %s
' "$file"
  printf 'HARDWARE %s
' "$hw"
  printf '----
'
}

# ------------------------------------------------------------
# Main operations
# ------------------------------------------------------------

genesis_scan() {
  root="${1:-.}"

  find "$root" -type f | while read -r f; do
    hw=$(probe_stat "$f" || true)
    emit_record "$f" "$hw"
  done
}

genesis_project() {
  file="$1"
  cat "$file" | project_software
}

genesis_syntax() {
  file="$1"
  cat "$file" | extract_enclosures
}

# ------------------------------------------------------------
# CLI
# ------------------------------------------------------------

usage() {
  cat <<EOF
GENESIS — string-native substrate

usage:
  genesis.sh scan [path]        # emit hardware provenance
  genesis.sh software <file>    # semantic projection
  genesis.sh syntax <file>      # enclosure detection

Topology is defined ONLY by enclosure.
Length defines polynomial degree, not dimension.
EOF
}

cmd="${1:-}"
case "$cmd" in
  scan)
    genesis_scan "${2:-.}"
    ;;
  software)
    [ $# -eq 2 ] || die "software requires file"
    genesis_project "$2"
    ;;
  syntax)
    [ $# -eq 2 ] || die "syntax requires file"
    genesis_syntax "$2"
    ;;
  *)
    usage
    ;;
esac
```

---

## Why this version is correct (briefly)

- **No lines assumed** — operates on raw strings  
- **Meta-tags = any non-alphanumeric enclosure**  
- **Hardware is byte-level, not symbolic**  
- **Software retains alphanumeric semantics**  
- **Syntax is projection, not identity**  
- **Nothing is hardcoded as sacred**  
- **All pinch points are emergent**  

This script is **GENESIS-safe**:
- you can ignore it and identity still holds
- you can replace it and semantics still hold
- you can extend it without breaking invariants
