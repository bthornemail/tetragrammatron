#!/usr/bin/env sh
# GENESIS Protocol bootstrap (POSIX sh)
# Usage:
#   ./genesis.sh init [DIR]
#   ./genesis.sh scan [DIR] > genesis.jsonl
#   ./genesis.sh view-terms [FILE] [terms|seps]
#   ./genesis.sh atom [PATH]
#
# Identity is filesystem reality: (dev, ino, mode, uid, gid) + path binding.
# Views are separators/markup and MUST NOT participate in identity.

set -eu

cmd="${1:-}"
root="${2:-.}"

die() { printf "%s\n" "$*" >&2; exit 1; }

need_cmd() { command -v "$1" >/dev/null 2>&1 || die "missing required command: $1"; }

need_cmd find
need_cmd awk
need_cmd sed
need_cmd date

stat_line() {
  p="$1"
  if stat -Lc '%d %i %f %u %g %s %X %Y %Z' "$p" >/dev/null 2>&1; then
    stat -Lc '%d %i %f %u %g %s %X %Y %Z' "$p"
  elif stat -Lf '%d %i %p %u %g %z %a %m %c' "$p" >/dev/null 2>&1; then
    stat -Lf '%d %i %p %u %g %z %a %m %c' "$p"
  else
    die "stat(1) format not supported on this system"
  fi
}

json_escape() {
  printf "%s" "$1" | sed     -e 's/\\/\\\\/g'     -e 's/"/\\"/g'     -e 's/\t/\\t/g'     -e 's/\r/\\r/g'     -e ':a;N;$!ba;s/\n/\\n/g'
}

now_iso() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }

view_terms() {
  file="$1"
  mode="${2:-terms}" # terms|seps
  if [ "$mode" = "terms" ]; then
    sed -e 's/[^A-Za-z0-9.]/ /g' "$file" | awk '{$1=$1;print}'
  elif [ "$mode" = "seps" ]; then
    sed -e 's/[A-Za-z0-9.]/ /g' "$file" | awk '{$1=$1;print}'
  else
    die "view-terms mode must be: terms|seps"
  fi
}

atom_json() {
  p="$1"
  ts="$(now_iso)"
  set -- $(stat_line "$p")
  dev="$1"; ino="$2"; mode="$3"; uid="$4"; gid="$5"; size="$6"; atime="$7"; mtime="$8"; ctime="$9"
  ep="$(json_escape "$p")"

  printf '{"t":"%s","k":"genesis.atom","v":{"path":"%s","stat":{"dev":"%s","ino":"%s","mode":"%s","uid":"%s","gid":"%s","size":"%s","atime":"%s","mtime":"%s","ctime":"%s"}}}\n'     "$ts" "$ep" "$dev" "$ino" "$mode" "$uid" "$gid" "$size" "$atime" "$mtime" "$ctime"
}

scan_dir() {
  dir="$1"
  inc="$dir/.genesisinclude"
  ign="$dir/.genesisignore"

  find "$dir" -type f 2>/dev/null | awk -v INC="$inc" -v IGN="$ign" '
    function load_patterns(path, arr,   line,n) {
      n=0
      while ((getline line < path) > 0) {
        sub(/^[ \t]+/,"",line); sub(/[ \t]+$/,"",line)
        if (line=="" || line ~ /^#/) continue
        arr[++n]=line
      }
      close(path)
      return n
    }
    function match_any(p, arr, n,   i, pat) {
      for (i=1;i<=n;i++) {
        pat=arr[i]
        gsub(/\./,"\\.",pat)
        gsub(/\*/,".*",pat)
        gsub(/\?/,".",pat)
        if (p ~ pat) return 1
      }
      return 0
    }
    BEGIN{
      ninc=0; nign=0
      if ((getline < INC) >= 0) { close(INC); ninc=load_patterns(INC, incp) }
      if ((getline < IGN) >= 0) { close(IGN); nign=load_patterns(IGN, ignp) }
    }
    {
      p=$0
      if (ninc>0 && !match_any(p, incp, ninc)) next
      if (nign>0 && match_any(p, ignp, nign)) next
      print p
    }
  ' | while IFS= read -r p; do atom_json "$p"; done
}

init_dir() {
  dir="$1"
  [ -d "$dir" ] || die "not a directory: $dir"

  cat >"$dir/README.org" <<'EOF'
#+TITLE: README
#+SUBTITLE: GENESIS bootstrap
#+PROPERTY: GENESIS_LAYER 1
#+PROPERTY: ROLE public
#+PROPERTY: STATUS seed

* Purpose
This repo is frozen and indexed by GENESIS Layer -1/0 atoms (filesystem stat) and viewed through higher-layer projections (Org, Canvas, VM).

* Quickstart
- Run: ./genesis.sh scan . > genesis.jsonl
- Add include/ignore patterns: .genesisinclude / .genesisignore
- Treat separators as views, not identity.
EOF

  cat >"$dir/AGENTS.org" <<'EOF'
#+TITLE: AGENTS
#+PROPERTY: GENESIS_LAYER 1
#+PROPERTY: ROLE protected
#+PROPERTY: STATUS seed

* Protocol
- Agents MUST NOT alter Layer -1/0 identity rules.
- Agents MAY add Layer 2/3 views (Org blocks, emitters).
- Agents MUST keep projections reversible where possible.

* Responsibilities
- Maintain .genesisinclude / .genesisignore patterns.
- Maintain GENESIS.org (authority) and genesis.org (implementation shadow).
EOF

  cat >"$dir/MANIFESTO.org" <<'EOF'
#+TITLE: MANIFESTO
#+PROPERTY: GENESIS_LAYER 1
#+PROPERTY: ROLE public
#+PROPERTY: STATUS seed

* One-page manifesto
Identity is what exists (inode/stat). Meaning is what we project (views). Execution is optional (emitters). Consensus is a lattice of compatible projections.
EOF

  cat >"$dir/INDEX.org" <<'EOF'
#+TITLE: INDEX
#+PROPERTY: GENESIS_LAYER 1
#+PROPERTY: ROLE public
#+PROPERTY: STATUS seed

* Virtual index
If INDEX.org is absent, the index is virtual: README/AGENTS/MANIFESTO/GENESIS imply the index structure.
EOF

  cat >"$dir/INCLUDE.org" <<'EOF'
#+TITLE: INCLUDE
#+PROPERTY: GENESIS_LAYER 1
#+PROPERTY: ROLE private
#+PROPERTY: STATUS seed

* Include patterns
Mirror of .genesisinclude (human readable).
EOF

  cat >"$dir/IGNORE.org" <<'EOF'
#+TITLE: IGNORE
#+PROPERTY: GENESIS_LAYER 1
#+PROPERTY: ROLE private
#+PROPERTY: STATUS seed

* Ignore patterns
Mirror of .genesisignore (human readable).
EOF

  cat >"$dir/GENESIS.org" <<'EOF'
#+TITLE: GENESIS
#+SUBTITLE: Authority specification
#+PROPERTY: GENESIS_LAYER 2
#+PROPERTY: ROLE public
#+PROPERTY: STATUS seed

* GENESIS layers (locked)
- Layer -1: PATH template (virtual), zero polynomial 00:00:00:00
- Layer 0: atom = filesystem stat (dev, ino, mode, uid, gid) + path binding
- Layer 1: bijection binding between atom and repository views (git/org/vm)
- Layer 2: syntax views (Org blocks, meta tags, emoji wrapper algebra)
- Layer 3: emission (tangle/export/compile); optional
- Layer 7: judgement/collapse (dedupe/merge by identity)

* Canonical constraints
** Hardware constraint (ground truth)
00:00:00:00

** Software constraint (tokenization / boundary)
- Terms view:  s/[^A-Za-z0-9.]/ /g
- Separators view (operators): s/[A-Za-z0-9.]/ /g

* Meta-operator alphabet
Meta tag operators MUST be composed only of characters outside [A-Za-z0-9.].
Examples: [[...]], <<...>>, ♤♡◇♧...♧◇♡♤
These are views and MUST NOT affect identity.

* Interfaces
** scan
Produces JSONL atoms without reading file contents.

** view-terms
Projects a file into (terms) or (seps) view.
EOF

  cat >"$dir/genesis.org" <<'EOF'
#+TITLE: genesis
#+SUBTITLE: Implementation shadow
#+PROPERTY: GENESIS_LAYER 3
#+PROPERTY: ROLE protected
#+PROPERTY: STATUS seed

* Shell implementation
This repo ships with ./genesis.sh which implements:
- init
- scan
- view-terms (terms|seps)
- atom

* Future: adapters
- Guile adapter for richer parsing
- C adapter for embedded (ESP32) schema tables
- emit-canvas adapter to generate Obsidian Canvas maps
EOF

  cat >"$dir/CONVERSATION.org" <<'EOF'
#+TITLE: CONVERSATION
#+PROPERTY: GENESIS_LAYER 1
#+PROPERTY: ROLE private
#+PROPERTY: STATUS seed

* Capture
Paste the provenance discussion here. Treat it as append-only.
EOF

  cat >"$dir/REVELATION.org" <<'EOF'
#+TITLE: REVELATION
#+PROPERTY: GENESIS_LAYER 2
#+PROPERTY: ROLE public
#+PROPERTY: STATUS seed

* Narrative layer
A story-safe place for metaphysical framing. Must not change Layer 0 rules.
EOF

  cat >"$dir/SCHEMA.org" <<'EOF'
#+TITLE: SCHEMA
#+PROPERTY: GENESIS_LAYER 2
#+PROPERTY: ROLE protected
#+PROPERTY: STATUS seed

* Schema
Describe address-schema.yaml and schema.bin negotiation here (later layers).
EOF

  cat >"$dir/SCHEME.org" <<'EOF'
#+TITLE: SCHEME
#+PROPERTY: GENESIS_LAYER 2
#+PROPERTY: ROLE protected
#+PROPERTY: STATUS seed

* Scheme blocks
Place Guile/R5RS/AAL/CanISA related blocks here.
EOF

  : >"$dir/.genesisinclude"
  cat >"$dir/.genesisignore" <<'EOF'
# Default ignore
.git/*
node_modules/*
dist/*
build/*
*.o
*.a
*.so
*.dylib
*.bin
EOF

  printf "Initialized GENESIS seed in: %s\n" "$dir" >&2
  printf "Next: ./genesis.sh scan %s > genesis.jsonl\n" "$dir" >&2
}

case "$cmd" in
  init) init_dir "$root" ;;
  scan) scan_dir "$root" ;;
  view-terms)
    f="${2:-}"; mode="${3:-terms}"
    [ -n "$f" ] || die "usage: ./genesis.sh view-terms FILE [terms|seps]"
    view_terms "$f" "$mode"
    ;;
  atom)
    p="${2:-}"; [ -n "$p" ] || die "usage: ./genesis.sh atom PATH"
    atom_json "$p"
    ;;
  *)
    die "usage:
  ./genesis.sh init [DIR]
  ./genesis.sh scan [DIR] > genesis.jsonl
  ./genesis.sh atom PATH
  ./genesis.sh view-terms FILE [terms|seps]"
    ;;
esac
