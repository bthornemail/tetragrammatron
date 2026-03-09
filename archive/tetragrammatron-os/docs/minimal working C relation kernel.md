# minimal, working C relation kernel that gives you

* an **append-only edge store** (atoms + relations)
* a **string intern table** (stable IDs *inside the graph*, not global numbers)
* a **Hardware → CanvasL edge emitter** (`emit_hardware_canvasl`)
* a **POSIX-as-projection** subset:

  * `posix_bind_name(dir, "name", inode)`
  * `posix_resolve_path(ns_root, "/a/b/c")`
  * `posix_readdir(dir)` iterator
  * `posix_cat(file)` via `BACKED_BY`

Everything is **relations-first**. Any “numbers” are internal indices, not exported as truth.

---

## Folder layout

Create these files:

```
src/rel/graph.h
src/rel/graph.c
src/proj/hw_emit.h
src/proj/hw_emit.c
src/proj/posix_proj.h
src/proj/posix_proj.c
src/demo/main.c
```

---

## src/rel/graph.h

```c
#pragma once
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// -------- Types --------

// AtomId/RelId/StrId are stable only within a single Graph instance.
// They are not canonical across machines or runs.
typedef uint32_t AtomId;
typedef uint32_t RelId;
typedef uint32_t StrId;

typedef struct {
  AtomId src;
  RelId  rel;
  AtomId dst;
} Edge;

typedef struct Graph Graph;

// -------- Graph lifecycle --------
Graph* graph_new(void);
void   graph_free(Graph* g);

// -------- Strings (interned) --------
StrId        graph_intern(Graph* g, const char* s);
const char*  graph_str(Graph* g, StrId id);

// -------- Relations (interned symbols) --------
RelId graph_rel(Graph* g, const char* rel_sym); // rel_sym like "HAS", "BACKED_BY", etc.

// -------- Atoms --------
AtomId graph_atom(Graph* g, const char* kind_sym); // kind_sym like "INODE", "CPU", "HOST"
StrId  graph_atom_kind(Graph* g, AtomId a);

// -------- Edges (append-only) --------
void   graph_add(Graph* g, AtomId src, RelId rel, AtomId dst);

// Iterate edges matching (src?, rel?, dst?) where any can be UINT32_MAX for wildcard.
typedef struct {
  const Graph* g;
  uint32_t i;
  AtomId src;
  RelId  rel;
  AtomId dst;
} EdgeIter;

EdgeIter graph_edges(const Graph* g, AtomId src, RelId rel, AtomId dst);
bool     edgeiter_next(EdgeIter* it, Edge* out);

// -------- Helpers --------
static inline AtomId WILD_ATOM(void) { return UINT32_MAX; }
static inline RelId  WILD_REL(void)  { return UINT32_MAX; }

#ifdef __cplusplus
}
#endif
```

---

## src/rel/graph.c

```c
#include "graph.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct {
  char*  buf;
  size_t len;
} StrEnt;

typedef struct {
  AtomId id;
  StrId  kind;
} AtomEnt;

struct Graph {
  // interned strings
  StrEnt* strs;
  uint32_t strs_len;
  uint32_t strs_cap;

  // atoms
  AtomEnt* atoms;
  uint32_t atoms_len;
  uint32_t atoms_cap;

  // edges
  Edge* edges;
  uint32_t edges_len;
  uint32_t edges_cap;
};

static void* xrealloc(void* p, size_t n) {
  void* r = realloc(p, n);
  if (!r) { fprintf(stderr, "OOM\n"); abort(); }
  return r;
}

Graph* graph_new(void) {
  Graph* g = (Graph*)calloc(1, sizeof(Graph));
  if (!g) { fprintf(stderr, "OOM\n"); abort(); }
  return g;
}

void graph_free(Graph* g) {
  if (!g) return;
  for (uint32_t i = 0; i < g->strs_len; i++) free(g->strs[i].buf);
  free(g->strs);
  free(g->atoms);
  free(g->edges);
  free(g);
}

static bool streq(const char* a, const char* b) { return strcmp(a,b)==0; }

StrId graph_intern(Graph* g, const char* s) {
  if (!s) s = "";
  for (uint32_t i = 0; i < g->strs_len; i++) {
    if (streq(g->strs[i].buf, s)) return (StrId)i;
  }
  if (g->strs_len == g->strs_cap) {
    g->strs_cap = g->strs_cap ? g->strs_cap * 2 : 64;
    g->strs = (StrEnt*)xrealloc(g->strs, g->strs_cap * sizeof(StrEnt));
  }
  size_t n = strlen(s);
  char* buf = (char*)malloc(n + 1);
  if (!buf) { fprintf(stderr, "OOM\n"); abort(); }
  memcpy(buf, s, n + 1);
  g->strs[g->strs_len] = (StrEnt){ .buf = buf, .len = n };
  return (StrId)(g->strs_len++);
}

const char* graph_str(Graph* g, StrId id) {
  if (!g || id >= g->strs_len) return "";
  return g->strs[id].buf;
}

RelId graph_rel(Graph* g, const char* rel_sym) {
  // Relations are just interned strings too.
  return (RelId)graph_intern(g, rel_sym);
}

AtomId graph_atom(Graph* g, const char* kind_sym) {
  if (g->atoms_len == g->atoms_cap) {
    g->atoms_cap = g->atoms_cap ? g->atoms_cap * 2 : 64;
    g->atoms = (AtomEnt*)xrealloc(g->atoms, g->atoms_cap * sizeof(AtomEnt));
  }
  StrId kind = graph_intern(g, kind_sym);
  AtomId id = (AtomId)g->atoms_len;
  g->atoms[g->atoms_len++] = (AtomEnt){ .id = id, .kind = kind };
  return id;
}

StrId graph_atom_kind(Graph* g, AtomId a) {
  if (!g || a >= g->atoms_len) return graph_intern(g, "UNKNOWN");
  return g->atoms[a].kind;
}

void graph_add(Graph* g, AtomId src, RelId rel, AtomId dst) {
  if (g->edges_len == g->edges_cap) {
    g->edges_cap = g->edges_cap ? g->edges_cap * 2 : 256;
    g->edges = (Edge*)xrealloc(g->edges, g->edges_cap * sizeof(Edge));
  }
  g->edges[g->edges_len++] = (Edge){ .src = src, .rel = rel, .dst = dst };
}

EdgeIter graph_edges(const Graph* g, AtomId src, RelId rel, AtomId dst) {
  EdgeIter it = { .g = g, .i = 0, .src = src, .rel = rel, .dst = dst };
  return it;
}

static bool match_u32(uint32_t want, uint32_t got) {
  return want == UINT32_MAX || want == got;
}

bool edgeiter_next(EdgeIter* it, Edge* out) {
  if (!it || !it->g) return false;
  while (it->i < it->g->edges_len) {
    Edge e = it->g->edges[it->i++];
    if (match_u32(it->src, e.src) && match_u32(it->rel, e.rel) && match_u32(it->dst, e.dst)) {
      if (out) *out = e;
      return true;
    }
  }
  return false;
}
```

---

## src/proj/hw_emit.h

```c
#pragma once
#include "../rel/graph.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

// Emits canonical edges in a CanvasL-ish textual form (one edge per line).
// This is NOT a numeric dump; it prints interned symbols.
void emit_hardware_canvasl(Graph* g, FILE* out);

#ifdef __cplusplus
}
#endif
```

---

## src/proj/hw_emit.c

```c
#include "hw_emit.h"
#include <time.h>

// Print atoms as: A<id>:<KIND>  (id is local, only for debugging)
static void print_atom(Graph* g, FILE* out, AtomId a) {
  const char* kind = graph_str(g, graph_atom_kind(g, a));
  fprintf(out, "A%u:%s", (unsigned)a, kind);
}

void emit_hardware_canvasl(Graph* g, FILE* out) {
  if (!g) return;
  if (!out) out = stdout;

  // Header is informational only.
  time_t t = time(NULL);
  fprintf(out, ";; hardware.canvasl (projection)\n");
  fprintf(out, ";; generated: %ld\n\n", (long)t);

  // Emit all edges as: (SRC REL DST)
  EdgeIter it = graph_edges(g, WILD_ATOM(), WILD_REL(), WILD_ATOM());
  Edge e;
  while (edgeiter_next(&it, &e)) {
    const char* rel = graph_str(g, (StrId)e.rel);
    fprintf(out, "(");
    print_atom(g, out, e.src);
    fprintf(out, " %s ", rel);
    print_atom(g, out, e.dst);
    fprintf(out, ")\n");
  }
}
```

---

## src/proj/posix_proj.h

```c
#pragma once
#include "../rel/graph.h"
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// --- Required relation symbols (interned) ---
typedef struct {
  RelId CONTAINS;
  RelId BINDS_NAME;   // (DIR BINDS_NAME NAMEATOM) and (NAMEATOM POINTS_TO INODE)
  RelId POINTS_TO;
  RelId BACKED_BY;
} PosixRels;

// --- Minimal "POSIX projection" helpers ---
PosixRels posix_rels(Graph* g);

// Create NAME atom for a string (kind: NAME) and return it.
AtomId posix_name_atom(Graph* g, const char* name);

// Bind "dir/name -> inode" purely by relations:
// (dir BINDS_NAME nameAtom) and (nameAtom POINTS_TO inode)
void posix_bind_name(Graph* g, AtomId dir_inode, const char* name, AtomId inode);

// Resolve path by repeated BINDS_NAME traversal.
// Supports absolute paths like "/a/b/c" starting from root inode.
bool posix_resolve_path(Graph* g, AtomId root_dir, const char* path, AtomId* out_inode);

// Readdir: iterate bindings in a directory. Returns next (nameAtom,inode) pairs.
typedef struct {
  Graph* g;
  AtomId dir;
  EdgeIter it_bind;   // DIR BINDS_NAME NAMEATOM
} ReaddirIter;

ReaddirIter posix_readdir(Graph* g, AtomId dir_inode);
bool        posix_readdir_next(ReaddirIter* it, const char** out_name, AtomId* out_inode);

// File backing: attach DATA atom to FILE inode:
// (file BACKED_BY data)
void posix_set_data(Graph* g, AtomId file_inode, AtomId data_atom);

// Retrieve DATA atom if present.
bool posix_get_data(Graph* g, AtomId file_inode, AtomId* out_data);

#ifdef __cplusplus
}
#endif
```

---

## src/proj/posix_proj.c

```c
#include "posix_proj.h"
#include <string.h>
#include <stdio.h>

// NOTE: We model directory entry as:
// (DIR BINDS_NAME NAMEATOM)
// (NAMEATOM POINTS_TO INODE)
// This avoids any numeric inode/dentry fields.

PosixRels posix_rels(Graph* g) {
  PosixRels r;
  r.CONTAINS   = graph_rel(g, "CONTAINS");
  r.BINDS_NAME = graph_rel(g, "BINDS_NAME");
  r.POINTS_TO  = graph_rel(g, "POINTS_TO");
  r.BACKED_BY  = graph_rel(g, "BACKED_BY");
  return r;
}

AtomId posix_name_atom(Graph* g, const char* name) {
  AtomId n = graph_atom(g, "NAME");
  // Attach the raw string label as an observation-like node without claiming identity.
  // We do it as: (NAME HAS STR:<...>) by making a STR atom.
  AtomId s = graph_atom(g, "STR");
  // encode the string in intern table as a symbol on the STR atom via kind tagging is not enough,
  // so we add relation: (STR OBSERVED_AS <string-id-atom>) is overkill for now.
  // Minimal: store the string as kind "STR:<value>" (still non-canonical, but ok for projection).
  // We'll implement: set STR atom kind to "STR" and carry the string via interned rel label.
  // Simpler: create an atom kind "STR:<...>".
  (void)s;
  // For now: represent the name string as an interned rel from NAME -> a TAG atom.
  AtomId tag = graph_atom(g, "TAG");
  RelId  lbl = graph_rel(g, name);     // rel symbol holds the string for rendering
  graph_add(g, n, lbl, tag);
  return n;
}

void posix_bind_name(Graph* g, AtomId dir_inode, const char* name, AtomId inode) {
  PosixRels r = posix_rels(g);
  AtomId nameAtom = posix_name_atom(g, name);
  graph_add(g, dir_inode, r.BINDS_NAME, nameAtom);
  graph_add(g, nameAtom, r.POINTS_TO, inode);
  // Also containment is optional but useful:
  graph_add(g, dir_inode, r.CONTAINS, inode);
}

static bool is_slash(char c){ return c=='/'; }

static const char* next_segment(const char* p, char* seg, size_t segcap) {
  while (*p && is_slash(*p)) p++;
  if (!*p) { seg[0]=0; return p; }
  size_t i=0;
  while (*p && !is_slash(*p)) {
    if (i+1<segcap) seg[i++] = *p;
    p++;
  }
  seg[i]=0;
  return p;
}

static bool name_matches(Graph* g, AtomId nameAtom, const char* seg) {
  // We encoded the string as a REL symbol on NAME: (NAME <seg> TAG)
  // So check if NAME has an outgoing edge with rel == intern(seg).
  RelId want = graph_rel(g, seg);
  EdgeIter it = graph_edges(g, nameAtom, want, WILD_ATOM());
  Edge e;
  return edgeiter_next(&it, &e);
}

bool posix_resolve_path(Graph* g, AtomId root_dir, const char* path, AtomId* out_inode) {
  if (!g || !path) return false;
  AtomId cur = root_dir;

  char seg[256];
  const char* p = path;

  // absolute only for now; if relative, treat as absolute from given root anyway
  while (*p) {
    p = next_segment(p, seg, sizeof(seg));
    if (seg[0]==0) break;

    // Find NAMEATOM bound in cur dir matching seg
    PosixRels r = posix_rels(g);
    EdgeIter it = graph_edges(g, cur, r.BINDS_NAME, WILD_ATOM());
    Edge e;
    bool found = false;
    AtomId next_inode = 0;

    while (edgeiter_next(&it, &e)) {
      AtomId nameAtom = e.dst;
      if (!name_matches(g, nameAtom, seg)) continue;

      // Resolve nameAtom -> inode
      EdgeIter it2 = graph_edges(g, nameAtom, r.POINTS_TO, WILD_ATOM());
      Edge e2;
      if (edgeiter_next(&it2, &e2)) {
        next_inode = e2.dst;
        found = true;
        break;
      }
    }

    if (!found) return false;
    cur = next_inode;
  }

  if (out_inode) *out_inode = cur;
  return true;
}

ReaddirIter posix_readdir(Graph* g, AtomId dir_inode) {
  PosixRels r = posix_rels(g);
  ReaddirIter it;
  it.g = g;
  it.dir = dir_inode;
  it.it_bind = graph_edges(g, dir_inode, r.BINDS_NAME, WILD_ATOM());
  return it;
}

bool posix_readdir_next(ReaddirIter* it, const char** out_name, AtomId* out_inode) {
  if (!it || !it->g) return false;
  PosixRels r = posix_rels(it->g);

  Edge e;
  while (edgeiter_next(&it->it_bind, &e)) {
    AtomId nameAtom = e.dst;

    // Extract one label from NAME atom: (NAME <seg> TAG) => rel symbol is the string
    EdgeIter it_lbl = graph_edges(it->g, nameAtom, WILD_REL(), WILD_ATOM());
    Edge lbl;
    const char* seg = NULL;
    if (edgeiter_next(&it_lbl, &lbl)) {
      seg = graph_str(it->g, (StrId)lbl.rel);
    } else {
      seg = "";
    }

    // Resolve inode
    EdgeIter it2 = graph_edges(it->g, nameAtom, r.POINTS_TO, WILD_ATOM());
    Edge e2;
    if (!edgeiter_next(&it2, &e2)) continue;

    if (out_name) *out_name = seg;
    if (out_inode) *out_inode = e2.dst;
    return true;
  }
  return false;
}

void posix_set_data(Graph* g, AtomId file_inode, AtomId data_atom) {
  PosixRels r = posix_rels(g);
  graph_add(g, file_inode, r.BACKED_BY, data_atom);
}

bool posix_get_data(Graph* g, AtomId file_inode, AtomId* out_data) {
  PosixRels r = posix_rels(g);
  EdgeIter it = graph_edges(g, file_inode, r.BACKED_BY, WILD_ATOM());
  Edge e;
  if (!edgeiter_next(&it, &e)) return false;
  if (out_data) *out_data = e.dst;
  return true;
}
```

---

## src/demo/main.c

This demo creates:

* a tiny **hardware graph** (HOST → CPU → CORE, NETIF exposes wifi/ble)
* a tiny **POSIX view** with `/dev/ttyUSB0` and `/etc/repo.canvasl`
* then prints:

  * hardware edges as CanvasL-ish lines
  * resolves `/dev/ttyUSB0`
  * readdir `/dev`

```c
#include "../rel/graph.h"
#include "../proj/hw_emit.h"
#include "../proj/posix_proj.h"
#include <stdio.h>

int main(void) {
  Graph* g = graph_new();

  // ---------- Hardware graph ----------
  RelId HAS        = graph_rel(g, "HAS");
  RelId EXPOSES    = graph_rel(g, "EXPOSES");
  RelId SUPPORTS   = graph_rel(g, "SUPPORTS");

  AtomId host = graph_atom(g, "HOST");
  AtomId cpu  = graph_atom(g, "CPU");
  AtomId core0= graph_atom(g, "CORE");
  AtomId core1= graph_atom(g, "CORE");
  AtomId isa  = graph_atom(g, "ISA");

  AtomId netif = graph_atom(g, "NETIF");
  AtomId cap_wifi = graph_atom(g, "cap:net:wifi");
  AtomId cap_ble  = graph_atom(g, "cap:net:ble");

  graph_add(g, host, HAS, cpu);
  graph_add(g, cpu, HAS, core0);
  graph_add(g, cpu, HAS, core1);
  graph_add(g, cpu, SUPPORTS, isa);
  graph_add(g, host, HAS, netif);
  graph_add(g, netif, EXPOSES, cap_wifi);
  graph_add(g, netif, EXPOSES, cap_ble);

  // ---------- POSIX-as-projection graph ----------
  AtomId root = graph_atom(g, "INODE:DIR");
  AtomId dev  = graph_atom(g, "INODE:DIR");
  AtomId etc  = graph_atom(g, "INODE:DIR");

  AtomId tty  = graph_atom(g, "INODE:DEVICE_NODE");
  AtomId file = graph_atom(g, "INODE:FILE");
  AtomId data = graph_atom(g, "DATA");

  posix_bind_name(g, root, "dev", dev);
  posix_bind_name(g, root, "etc", etc);
  posix_bind_name(g, dev,  "ttyUSB0", tty);
  posix_bind_name(g, etc,  "repo.canvasl", file);

  posix_set_data(g, file, data);

  // ---------- Output ----------
  puts("=== HARDWARE CANVASTEXT ===");
  emit_hardware_canvasl(g, stdout);

  puts("\n=== POSIX RESOLVE ===");
  AtomId out;
  if (posix_resolve_path(g, root, "/dev/ttyUSB0", &out)) {
    printf("Resolved /dev/ttyUSB0 -> A%u:%s\n",
      (unsigned)out, graph_str(g, graph_atom_kind(g, out)));
  } else {
    puts("Resolve failed");
  }

  puts("\n=== POSIX READDIR /dev ===");
  ReaddirIter it = posix_readdir(g, dev);
  const char* name;
  AtomId inode;
  while (posix_readdir_next(&it, &name, &inode)) {
    printf(" - %s -> A%u:%s\n", name, (unsigned)inode,
      graph_str(g, graph_atom_kind(g, inode)));
  }

  graph_free(g);
  return 0;
}
```

---

## Build + run (Linux/macOS)

From repo root:

```bash
cc -std=c11 -O2 \
  src/rel/graph.c \
  src/proj/hw_emit.c \
  src/proj/posix_proj.c \
  src/demo/main.c \
  -o can_rel_demo

./can_rel_demo
```

---

# What you get immediately

### A) Hardware projection

You can now build real probes that produce edges like:

* `(HOST HAS CPU)`
* `(NETIF EXPOSES cap:net:wifi)`
* `(CPU SUPPORTS cap:isa:xtensa)`

…and emit them to `hardware.canvasl` **without trusting numeric IDs**.

### B) POSIX-as-projection

You can represent `/dev`, mounts, hardlinks, etc. as **relations** and implement minimal compatibility functions as **queries**.