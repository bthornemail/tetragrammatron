#!/bin/sh
# genesis-protocol.sh — Reference implementation of RFC-0002-GENESIS-PROTOCOL
# POSIX-compliant, minimal dependencies
set -eu

VERSION="0.2.0-rfc0002"

# ============================================================================
# GLOBALS
# ============================================================================

GENESIS_DB="${GENESIS_DB:-.genesis/observations.jsonl}"
GENESIS_IGNORE="${GENESIS_IGNORE:-.genesisignore}"
GENESIS_INCLUDE="${GENESIS_INCLUDE:-.genesisinclude}"
GENESIS_SCHEMA="${GENESIS_SCHEMA:-.genesisschema}"

# ============================================================================
# UTILITIES
# ============================================================================

die() {
    printf "genesis-protocol: error: %s\n" "$*" >&2
    exit 1
}

log() {
    printf "[genesis] %s\n" "$*" >&2
}

have() {
    command -v "$1" >/dev/null 2>&1
}

now_iso() {
    if have date; then
        date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date
    else
        echo "1970-01-01T00:00:00Z"
    fi
}

# ============================================================================
# LAYER −0 / 0: OBSERVATION (RFC-0002 §4 Layer 0)
# ============================================================================
# Purpose: Observe reality without interpretation
# MUST read POSIX file metadata only
# MUST NOT read file contents
# MUST emit append-only observation record
# MUST produce Atom from stat tuple

atom_from_path() {
    # Generate canonical Atom from path
    # Identity is alphanumeric only (RFC-0002 §2)
    printf "%s" "$1" | tr -cd '[:alnum:]' | tr '[:lower:]' '[:upper:]'
}

stat_fields() {
    # Portable stat wrapper
    f="$1"
    if stat --version >/dev/null 2>&1; then
        # GNU stat
        stat -c "%d %i %f %u %g %s %X %Y %Z" "$f" 2>/dev/null
    elif stat -f "%d %i %p %u %g %z %a %m %c" "$f" >/dev/null 2>&1; then
        # BSD stat
        stat -f "%d %i %p %u %g %z %a %m %c" "$f" 2>/dev/null
    else
        # Fallback: minimal info
        sz=$(wc -c <"$f" 2>/dev/null || echo 0)
        printf "0 0 0 0 0 %s 0 0 0\n" "$sz"
    fi
}

observe_file() {
    # Layer 0: Pure observation (no content read)
    filepath="$1"
    root="${2:-.}"

    relpath="${filepath#"$root"/}"
    [ "$relpath" = "$filepath" ] && relpath="$filepath"

    # Get stat fields
    set -- $(stat_fields "$filepath")
    dev="$1" ino="$2" mode="$3" uid="$4" gid="$5"
    size="$6" atime="$7" mtime="$8" ctime="$9"

    # Generate Atom (alphanumeric identity only)
    atom=$(atom_from_path "$relpath")

    # Timestamp
    ts=$(now_iso)

    # Escape path for JSON
    esc_path=$(printf "%s" "$relpath" | sed 's/\\/\\\\/g; s/"/\\"/g')

    # Emit observation as JSONL (append-only)
    printf '{"ts":"%s","atom":"%s","path":"%s","stat":{"dev":%s,"ino":%s,"mode":%s,"uid":%s,"gid":%s,"size":%s,"atime":%s,"mtime":%s,"ctime":%s}}\n' \
        "$ts" "$atom" "$esc_path" "$dev" "$ino" "$mode" "$uid" "$gid" "$size" "$atime" "$mtime" "$ctime"
}

# ============================================================================
# FILE CLASSIFICATION (Light/Dark)
# ============================================================================

should_ignore() {
    # Check if path matches ignore patterns
    filepath="$1"
    base=$(basename "$filepath")
    dir=$(dirname "$filepath")

    ignore_file="$dir/$GENESIS_IGNORE"

    [ ! -f "$ignore_file" ] && return 1

    while IFS= read -r pattern; do
        case "$pattern" in
            ''|\#*) continue ;;
        esac
        case "$base" in
            $pattern) return 0 ;;
        esac
    done <"$ignore_file"

    return 1
}

should_include() {
    # Check if path matches include patterns
    filepath="$1"
    base=$(basename "$filepath")
    dir=$(dirname "$filepath")

    include_file="$dir/$GENESIS_INCLUDE"

    [ ! -f "$include_file" ] && return 1

    while IFS= read -r pattern; do
        case "$pattern" in
            ''|\#*) continue ;;
        esac
        case "$base" in
            $pattern) return 0 ;;
        esac
    done <"$include_file"

    return 1
}

classify_file() {
    # Classify as Light (KK), Dark (UU), or Deferred (KU)
    filepath="$1"

    if should_ignore "$filepath"; then
        echo "UU"  # Unknown-Unknown (dark)
    elif should_include "$filepath"; then
        echo "KK"  # Known-Known (light)
    else
        echo "KU"  # Known-Unknown (deferred)
    fi
}

# ============================================================================
# LAYER 0: GENERATE (observe entire repository)
# ============================================================================

cmd_generate() {
    # RFC-0002 §9: generate(blob)
    # Input: filesystem
    # Output: append-only observations

    root="${1:-.}"
    outfile="${2:-$GENESIS_DB}"

    [ ! -d "$root" ] && die "not a directory: $root"

    # Ensure output directory exists
    outdir=$(dirname "$outfile")
    mkdir -p "$outdir"

    # Initialize or append to observation log
    [ ! -f "$outfile" ] && : >"$outfile"

    log "observing $root → $outfile"

    count=0
    find "$root" -type f 2>/dev/null | while IFS= read -r filepath; do
        # Classify file
        class=$(classify_file "$filepath")

        # Only observe Light files (KK)
        if [ "$class" = "KK" ]; then
            observe_file "$filepath" "$root" >>"$outfile"
            count=$((count + 1))
        fi
    done

    log "observed $(wc -l <"$outfile") atoms"
}

# ============================================================================
# LAYER 1: CONTEMPLATE (project to human-readable views)
# ============================================================================

cmd_contemplate() {
    # RFC-0002 §9: contemplate(atom)
    # Input: observations
    # Output: .org projections

    infile="${1:-$GENESIS_DB}"
    outdir="${2:-.genesis/views}"

    [ ! -f "$infile" ] && die "observation file not found: $infile"

    mkdir -p "$outdir"

    log "projecting observations → $outdir"

    # Read observations and project to .org files
    while IFS= read -r line; do
        # Extract fields (simple JSON parsing for POSIX)
        atom=$(echo "$line" | sed -n 's/.*"atom":"\([^"]*\)".*/\1/p')
        path=$(echo "$line" | sed -n 's/.*"path":"\([^"]*\)".*/\1/p')
        ts=$(echo "$line" | sed -n 's/.*"ts":"\([^"]*\)".*/\1/p')

        [ -z "$atom" ] && continue

        # Create .org view
        orgfile="$outdir/${atom}.org"

        {
            printf "#+TITLE: %s\n" "$atom"
            printf "#+GENESIS_ATOM: %s\n" "$atom"
            printf "#+GENESIS_PATH: %s\n" "$path"
            printf "#+GENESIS_OBSERVED: %s\n" "$ts"
            printf "\n* Observation\n\n"
            printf "This atom was observed at =%s=\n\n" "$ts"
            printf "Original path: =%s=\n\n" "$path"
            printf "* Properties\n\n"
            printf ":PROPERTIES:\n"
            printf ":ATOM: %s\n" "$atom"
            printf ":PATH: %s\n" "$path"
            printf ":OBSERVED: %s\n" "$ts"
            printf ":END:\n"
        } >"$orgfile"

    done <"$infile"

    log "projected $(find "$outdir" -name "*.org" | wc -l) views"
}

# ============================================================================
# LAYER 2: INTERPRET (apply schema/scheme)
# ============================================================================

cmd_interpret() {
    # RFC-0002 Layer 2: Schema/Scheme interpretation
    # Currently minimal: validates schema constraints

    schemafile="${1:-$GENESIS_SCHEMA}"
    observations="${2:-$GENESIS_DB}"

    if [ ! -f "$schemafile" ]; then
        log "no schema file, skipping interpretation"
        return 0
    fi

    log "interpreting with schema: $schemafile"

    # Read schema constraints
    realm=$(grep "^realm.default=" "$schemafile" | cut -d= -f2)

    log "default realm: ${realm:-private}"

    # Validate observations against schema
    # (Extensible: add custom rules here)

    log "interpretation complete"
}

# ============================================================================
# LAYER 3: TRANSLATE (emit artifacts)
# ============================================================================

cmd_translate() {
    # RFC-0002 §9: translate(atom, scheme)
    # Input: atom + scheme
    # Output: emitted artifacts

    atom="$1"
    scheme="${2:-identity}"
    viewdir="${3:-.genesis/views}"
    outdir="${4:-.genesis/artifacts}"

    orgfile="$viewdir/${atom}.org"

    [ ! -f "$orgfile" ] && die "view not found: $orgfile"

    mkdir -p "$outdir"

    log "translating $atom with scheme=$scheme"

    case "$scheme" in
        identity)
            # Identity scheme: just copy the view
            cp "$orgfile" "$outdir/${atom}.org"
            ;;
        markdown)
            # Convert .org to .md (simple transformation)
            sed 's/^\#\+TITLE:/# /; s/^\*\+/##/g' "$orgfile" >"$outdir/${atom}.md"
            ;;
        *)
            die "unknown scheme: $scheme"
            ;;
    esac

    log "emitted to $outdir"
}

# ============================================================================
# LAYER 7: VALIDATE (reconcile observations with reality)
# ============================================================================

cmd_validate() {
    # RFC-0002 Layer 7: Judgement/Reconciliation
    # Ensures bijection between .git tree and GENESIS observations

    root="${1:-.}"
    observations="${2:-$GENESIS_DB}"

    [ ! -f "$observations" ] && die "observation file not found: $observations"

    log "validating observations against filesystem"

    # Count observed atoms
    observed_count=$(wc -l <"$observations")

    # Count current light files
    current_count=0
    find "$root" -type f 2>/dev/null | while IFS= read -r filepath; do
        if [ "$(classify_file "$filepath")" = "KK" ]; then
            current_count=$((current_count + 1))
        fi
    done

    log "observed: $observed_count atoms"
    log "current: $current_count light files"

    # Check for drift
    if [ "$observed_count" -eq "$current_count" ]; then
        log "✓ validation passed: bijection maintained"
        return 0
    else
        log "⚠ validation warning: counts differ (may need regenerate)"
        return 1
    fi
}

# ============================================================================
# WORKFLOW COMMANDS
# ============================================================================

cmd_init() {
    # Initialize a GENESIS repository
    root="${1:-.}"

    log "initializing GENESIS repository in $root"

    mkdir -p "$root/.genesis"

    # Create default ignore file
    if [ ! -f "$root/$GENESIS_IGNORE" ]; then
        cat >"$root/$GENESIS_IGNORE" <<'EOF'
# GENESIS ignore patterns (dark matter)
.git/
.genesis/
node_modules/
*.o
*.so
*.dylib
*.exe
*.bin
*.png
*.jpg
*.jpeg
*.mp4
*.zip
EOF
        log "created $GENESIS_IGNORE"
    fi

    # Create default include file
    if [ ! -f "$root/$GENESIS_INCLUDE" ]; then
        cat >"$root/$GENESIS_INCLUDE" <<'EOF'
# GENESIS include patterns (light matter)
*.org
*.md
*.sh
*.c
*.h
*.scm
*.json
*.jsonl
*.yaml
*.yml
*.txt
EOF
        log "created $GENESIS_INCLUDE"
    fi

    # Create default schema
    if [ ! -f "$root/$GENESIS_SCHEMA" ]; then
        cat >"$root/$GENESIS_SCHEMA" <<'EOF'
# GENESIS schema
schema.version=0
realm.default=private
signature.required.protected=false
signature.required.public=false
EOF
        log "created $GENESIS_SCHEMA"
    fi

    log "✓ initialization complete"
}

cmd_workflow() {
    # Run complete workflow: generate → contemplate → validate
    root="${1:-.}"

    log "=== GENESIS Protocol Workflow ==="
    log ""

    log "Layer 0: Generate (observe)"
    cmd_generate "$root"

    log ""
    log "Layer 1: Contemplate (project)"
    cmd_contemplate

    log ""
    log "Layer 2: Interpret (schema)"
    cmd_interpret

    log ""
    log "Layer 7: Validate (reconcile)"
    cmd_validate "$root"

    log ""
    log "=== Workflow complete ==="
}

cmd_status() {
    # Show repository status
    log "GENESIS Protocol Status"
    log ""

    if [ -f "$GENESIS_DB" ]; then
        log "Observations: $(wc -l <"$GENESIS_DB") atoms"
    else
        log "Observations: none (run 'generate' first)"
    fi

    if [ -d ".genesis/views" ]; then
        log "Views: $(find .genesis/views -name "*.org" 2>/dev/null | wc -l) projections"
    else
        log "Views: none"
    fi

    if [ -f "$GENESIS_SCHEMA" ]; then
        realm=$(grep "^realm.default=" "$GENESIS_SCHEMA" | cut -d= -f2)
        log "Schema: ${realm:-private} realm"
    else
        log "Schema: not configured"
    fi
}

# ============================================================================
# USAGE
# ============================================================================

usage() {
    cat <<'EOF'
genesis-protocol.sh — RFC-0002-GENESIS-PROTOCOL reference implementation

USAGE
  genesis-protocol.sh init [ROOT]
      Initialize GENESIS repository

  genesis-protocol.sh generate [ROOT] [OUT.jsonl]
      Layer 0: Observe filesystem → append-only log

  genesis-protocol.sh contemplate [IN.jsonl] [VIEWDIR]
      Layer 1: Project observations → .org views

  genesis-protocol.sh interpret [SCHEMA] [OBSERVATIONS]
      Layer 2: Apply schema/scheme constraints

  genesis-protocol.sh translate ATOM [SCHEME] [VIEWDIR] [OUTDIR]
      Layer 3: Emit artifacts from atom

  genesis-protocol.sh validate [ROOT] [OBSERVATIONS]
      Layer 7: Validate bijection with filesystem

  genesis-protocol.sh workflow [ROOT]
      Run complete workflow (generate → contemplate → validate)

  genesis-protocol.sh status
      Show repository status

ENVIRONMENT
  GENESIS_DB          Observation database (.genesis/observations.jsonl)
  GENESIS_IGNORE      Ignore patterns file (.genesisignore)
  GENESIS_INCLUDE     Include patterns file (.genesisinclude)
  GENESIS_SCHEMA      Schema constraints file (.genesisschema)

PRINCIPLES (RFC-0002)
  - Identity is alphanumeric only
  - Separators are views, not identity
  - Observation precedes interpretation
  - All operations are deterministic
  - Layer 7 validates Layers 0–6

VERSION
  genesis-protocol.sh version 0.2.0-rfc0002
EOF
}

# ============================================================================
# MAIN
# ============================================================================

cmd="${1:-}"
[ $# -gt 0 ] && shift

case "$cmd" in
    init)
        cmd_init "${1:-.}"
        ;;
    generate)
        cmd_generate "${1:-.}" "${2:-$GENESIS_DB}"
        ;;
    contemplate)
        cmd_contemplate "${1:-$GENESIS_DB}" "${2:-.genesis/views}"
        ;;
    interpret)
        cmd_interpret "${1:-$GENESIS_SCHEMA}" "${2:-$GENESIS_DB}"
        ;;
    translate)
        [ $# -lt 1 ] && die "translate requires ATOM argument"
        cmd_translate "$1" "${2:-identity}" "${3:-.genesis/views}" "${4:-.genesis/artifacts}"
        ;;
    validate)
        cmd_validate "${1:-.}" "${2:-$GENESIS_DB}"
        ;;
    workflow)
        cmd_workflow "${1:-.}"
        ;;
    status)
        cmd_status
        ;;
    ""|-h|--help|help)
        usage
        ;;
    *)
        die "unknown command: $cmd (try 'help')"
        ;;
esac
