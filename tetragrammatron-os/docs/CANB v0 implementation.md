Below is a **C-first, platform-independent** CANB v0 implementation: **decoder + VM core + minimal ISA** (graph-relations + stack + jumps). It’s written to be **embedded-friendly** (ESP32/Pico): no libc magic beyond `stdint.h`, `string.h`, optional `stdio.h` for debug.

You’ll get **four files**:

1. `canb.h` (public API + types)
2. `canb_decode.c` (CANB container + ULEB128 decode)
3. `canb_vm.c` (VM + stack + graph store + execution)
4. `canb_opcodes.h` (opcode constants)

This is enough to run `.canb` bytecode produced by your assembler later.

---

## `src/canb/c/canb_opcodes.h`

```c
#pragma once

// --- Core ---
#define CANB_OP_NOP          0x00
#define CANB_OP_HALT         0x01

#define CANB_OP_PUSH_ATOM    0x02  // A (uleb128 atom index)
#define CANB_OP_PUSH_BYTES   0x03  // B (uleb128 len + bytes)
#define CANB_OP_PUSH_TRUE    0x04
#define CANB_OP_PUSH_FALSE   0x05
#define CANB_OP_DROP         0x06
#define CANB_OP_DUP          0x07
#define CANB_OP_SWAP         0x08
#define CANB_OP_EQ           0x09

#define CANB_OP_JUMP         0x0A  // U (uleb128 absolute byte offset)
#define CANB_OP_JUMP_IF      0x0B  // U (uleb128 absolute byte offset), pops bool

// --- Graph / relations ---
#define CANB_OP_EDGE_ADD        0x10  // pop dst, rel, src (Atoms)
#define CANB_OP_EDGE_DEL        0x11  // pop dst, rel, src (Atoms)
#define CANB_OP_EDGE_HAS        0x12  // pop dst, rel, src -> push bool
#define CANB_OP_EDGE_FIND_SRD   0x13  // pop src, rel -> push dst or Nil (min by atom bytes)
#define CANB_OP_EDGE_FIND_RDD   0x14  // pop rel, dst -> push src or Nil (min by atom bytes)
#define CANB_OP_EDGE_ITER_SRD   0x15  // U, pop src, rel -> push up to U dst atoms (stable order)

// --- Optional session-local projection helpers ---
#define CANB_OP_FD_ALLOC      0x20  // pop inodeAtom -> push fdAtom (session token atom)
#define CANB_OP_FD_LOOKUP     0x21  // pop fdAtom -> push inodeAtom or Nil
#define CANB_OP_SET_CWD       0x22  // pop dirAtom
#define CANB_OP_GET_CWD       0x23  // push dirAtom

// --- Trace (optional; VM provides hooks) ---
#define CANB_OP_TRACE         0x30  // A (atom tag)
#define CANB_OP_TRACE_TOP     0x31
```

---

## `src/canb/c/canb.h`

```c
#pragma once
#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// =======================
// Errors
// =======================
typedef enum {
  CANB_OK = 0,
  CANB_ERR_BAD_MAGIC,
  CANB_ERR_UNSUPPORTED_VERSION,
  CANB_ERR_TRUNCATED,
  CANB_ERR_VARINT_OVERFLOW,
  CANB_ERR_BAD_ATOM,
  CANB_ERR_BAD_CODE,
  CANB_ERR_STACK_UNDERFLOW,
  CANB_ERR_STACK_OVERFLOW,
  CANB_ERR_TYPE,
  CANB_ERR_GRAPH_FULL,
  CANB_ERR_FD_FULL,
  CANB_ERR_BAD_JUMP,
} canb_err_t;

// =======================
// Atom table
// =======================
typedef enum {
  CANB_ATOM_SYMBOL_UTF8 = 0,
  CANB_ATOM_BLAKE3_32   = 1,
  CANB_ATOM_RAW_BYTES   = 2,
} canb_atom_kind_t;

typedef struct {
  uint8_t kind;       // canb_atom_kind_t
  uint32_t len;       // length of bytes
  const uint8_t* bytes; // pointer into the original buffer
} canb_atom_t;

// =======================
// Program view (decoded container)
// =======================
typedef struct {
  const uint8_t* buf;
  size_t len;

  uint8_t version;
  uint8_t flags;

  const canb_atom_t* atoms;
  uint32_t atom_count;

  const uint8_t* code;
  uint32_t code_len;
} canb_program_t;

// Decode without allocations: caller supplies atom array storage.
canb_err_t canb_decode_program(
  canb_program_t* out,
  const uint8_t* buf,
  size_t len,
  canb_atom_t* atom_storage,
  uint32_t atom_storage_cap
);

// =======================
// VM Values
// =======================
typedef enum {
  CANB_VAL_NIL = 0,
  CANB_VAL_BOOL,
  CANB_VAL_ATOM,
  CANB_VAL_BYTES,
} canb_val_kind_t;

typedef struct {
  uint8_t kind; // canb_val_kind_t
  union {
    uint8_t b;
    uint32_t atom_index; // index into program atom table
    struct { const uint8_t* p; uint32_t n; } bytes; // points into code buffer or external
  } as;
} canb_val_t;

// =======================
// Graph store (edges)
// =======================
typedef struct {
  uint32_t src;
  uint32_t rel;
  uint32_t dst;
} canb_edge_t;

typedef struct {
  canb_edge_t* edges;
  uint32_t cap;
  uint32_t len;
} canb_graph_t;

// =======================
// Session local mappings (FD, CWD)
// =======================
typedef struct {
  // fd tokens are session atoms in a separate table (we synthesize them as RAW_BYTES atoms)
  // For embedded simplicity: we store mapping in arrays of atom indices in the program atom table
  // by *reserving* a range of synthetic atom indices (not stored in the CANB file).
  //
  // If you prefer: keep separate token table and push bytes tokens instead of atoms.
  //
  // This implementation uses synthetic atom indices >= program.atom_count.
  uint32_t* fd_to_inode;  // length fd_cap, value is atom index (inode) or UINT32_MAX
  uint32_t fd_cap;
  uint32_t fd_next;

  uint32_t cwd_atom; // atom index or UINT32_MAX for nil
} canb_session_t;

// =======================
// VM configuration
// =======================
typedef struct {
  // Stack
  canb_val_t* stack;
  uint32_t stack_cap;

  // Graph
  canb_graph_t graph;

  // Session
  canb_session_t session;

  // Trace hooks (optional)
  void (*trace_atom)(void* user, const canb_program_t* P, uint32_t atom_index);
  void (*trace_val)(void* user, const canb_program_t* P, const canb_val_t* v);
  void* trace_user;

  // Synthetic atom base: any atom index >= P->atom_count is considered session-synthetic.
  // VM will never look it up in P->atoms.
} canb_vm_t;

// Initialize session (fd_to_inode array required).
void canb_session_init(canb_session_t* s, uint32_t* fd_to_inode, uint32_t fd_cap);

// Initialize graph with caller storage.
void canb_graph_init(canb_graph_t* g, canb_edge_t* edges, uint32_t cap);

// Run VM
typedef struct {
  canb_err_t err;
  uint32_t pc;      // byte offset in code where error occurred (or HALT)
  uint32_t steps;   // executed instruction count
} canb_run_result_t;

canb_run_result_t canb_vm_run(
  canb_vm_t* vm,
  const canb_program_t* P,
  uint32_t step_limit
);

// Helpers: atom comparison uses canonical bytes ordering.
// Returns -1/0/+1.
int canb_atom_cmp(const canb_program_t* P, uint32_t a_idx, uint32_t b_idx);

#ifdef __cplusplus
}
#endif
```

---

## `src/canb/c/canb_decode.c`

```c
#include "canb.h"
#include <string.h>

static canb_err_t read_u8(const uint8_t* buf, size_t len, size_t* off, uint8_t* out) {
  if (*off + 1 > len) return CANB_ERR_TRUNCATED;
  *out = buf[*off];
  *off += 1;
  return CANB_OK;
}

static canb_err_t read_u16_le(const uint8_t* buf, size_t len, size_t* off, uint16_t* out) {
  if (*off + 2 > len) return CANB_ERR_TRUNCATED;
  *out = (uint16_t)buf[*off] | ((uint16_t)buf[*off + 1] << 8);
  *off += 2;
  return CANB_OK;
}

// ULEB128 decode up to 32-bit
static canb_err_t read_uleb32(const uint8_t* buf, size_t len, size_t* off, uint32_t* out) {
  uint32_t result = 0;
  uint32_t shift = 0;
  for (int i = 0; i < 5; i++) {
    if (*off >= len) return CANB_ERR_TRUNCATED;
    uint8_t byte = buf[*off];
    (*off)++;

    uint32_t slice = (uint32_t)(byte & 0x7F);
    if (shift >= 32 && slice) return CANB_ERR_VARINT_OVERFLOW;

    result |= (slice << shift);
    if ((byte & 0x80) == 0) {
      *out = result;
      return CANB_OK;
    }
    shift += 7;
  }
  return CANB_ERR_VARINT_OVERFLOW;
}

canb_err_t canb_decode_program(
  canb_program_t* out,
  const uint8_t* buf,
  size_t len,
  canb_atom_t* atom_storage,
  uint32_t atom_storage_cap
) {
  if (!out || !buf || !atom_storage) return CANB_ERR_BAD_CODE;
  memset(out, 0, sizeof(*out));

  if (len < 8) return CANB_ERR_TRUNCATED;
  if (memcmp(buf, "CANB", 4) != 0) return CANB_ERR_BAD_MAGIC;

  size_t off = 4;

  uint8_t version = 0, flags = 0;
  if (read_u8(buf, len, &off, &version) != CANB_OK) return CANB_ERR_TRUNCATED;
  if (read_u8(buf, len, &off, &flags) != CANB_OK) return CANB_ERR_TRUNCATED;

  uint16_t reserved = 0;
  if (read_u16_le(buf, len, &off, &reserved) != CANB_OK) return CANB_ERR_TRUNCATED;

  // v0 only
  if (version != 0x00) return CANB_ERR_UNSUPPORTED_VERSION;

  uint32_t atom_count = 0;
  canb_err_t e = read_uleb32(buf, len, &off, &atom_count);
  if (e != CANB_OK) return e;
  if (atom_count > atom_storage_cap) return CANB_ERR_BAD_ATOM;

  for (uint32_t i = 0; i < atom_count; i++) {
    uint8_t kind = 0;
    e = read_u8(buf, len, &off, &kind);
    if (e != CANB_OK) return e;

    uint32_t alen = 0;
    e = read_uleb32(buf, len, &off, &alen);
    if (e != CANB_OK) return e;

    if (off + alen > len) return CANB_ERR_TRUNCATED;

    atom_storage[i].kind = kind;
    atom_storage[i].len = alen;
    atom_storage[i].bytes = &buf[off];
    off += alen;
  }

  uint32_t code_len = 0;
  e = read_uleb32(buf, len, &off, &code_len);
  if (e != CANB_OK) return e;

  if (off + code_len > len) return CANB_ERR_TRUNCATED;

  out->buf = buf;
  out->len = len;
  out->version = version;
  out->flags = flags;
  out->atoms = atom_storage;
  out->atom_count = atom_count;
  out->code = &buf[off];
  out->code_len = code_len;

  return CANB_OK;
}
```

---

## `src/canb/c/canb_vm.c`

```c
#include "canb.h"
#include "canb_opcodes.h"
#include <string.h>

#define CANB_NIL_ATOM UINT32_MAX

// --------------------
// Varint decode inside code stream (same as file uleb)
// --------------------
static canb_err_t code_read_uleb32(const uint8_t* code, uint32_t code_len, uint32_t* pc, uint32_t* out) {
  uint32_t result = 0;
  uint32_t shift = 0;
  for (int i = 0; i < 5; i++) {
    if (*pc >= code_len) return CANB_ERR_TRUNCATED;
    uint8_t byte = code[*pc];
    (*pc)++;

    uint32_t slice = (uint32_t)(byte & 0x7F);
    result |= (slice << shift);

    if ((byte & 0x80) == 0) {
      *out = result;
      return CANB_OK;
    }
    shift += 7;
  }
  return CANB_ERR_VARINT_OVERFLOW;
}

static canb_err_t code_read_bytes(const uint8_t* code, uint32_t code_len, uint32_t* pc, const uint8_t** p, uint32_t* n) {
  uint32_t len = 0;
  canb_err_t e = code_read_uleb32(code, code_len, pc, &len);
  if (e != CANB_OK) return e;
  if (*pc + len > code_len) return CANB_ERR_TRUNCATED;
  *p = &code[*pc];
  *n = len;
  *pc += len;
  return CANB_OK;
}

// --------------------
// Stack helpers
// --------------------
static canb_err_t push(canb_vm_t* vm, uint32_t* sp, canb_val_t v) {
  if (*sp >= vm->stack_cap) return CANB_ERR_STACK_OVERFLOW;
  vm->stack[*sp] = v;
  (*sp)++;
  return CANB_OK;
}

static canb_err_t pop(canb_vm_t* vm, uint32_t* sp, canb_val_t* out) {
  if (*sp == 0) return CANB_ERR_STACK_UNDERFLOW;
  (*sp)--;
  *out = vm->stack[*sp];
  return CANB_OK;
}

static canb_err_t peek(canb_vm_t* vm, uint32_t sp, canb_val_t* out) {
  if (sp == 0) return CANB_ERR_STACK_UNDERFLOW;
  *out = vm->stack[sp - 1];
  return CANB_OK;
}

// --------------------
// Atom ordering (stable iteration)
// - For synthetic atoms >= atom_count, compare by numeric index (session-local, stable within run).
// - For file atoms, compare by bytes lexicographically; tie-break by kind then len.
// --------------------
int canb_atom_cmp(const canb_program_t* P, uint32_t a_idx, uint32_t b_idx) {
  if (a_idx == b_idx) return 0;

  const uint32_t n = P->atom_count;
  const int a_syn = (a_idx >= n);
  const int b_syn = (b_idx >= n);

  if (a_syn && b_syn) return (a_idx < b_idx) ? -1 : +1;
  if (a_syn && !b_syn) return +1; // file atoms sort before synthetic by default
  if (!a_syn && b_syn) return -1;

  const canb_atom_t* A = &P->atoms[a_idx];
  const canb_atom_t* B = &P->atoms[b_idx];

  // primary: bytes lexicographic
  uint32_t min = (A->len < B->len) ? A->len : B->len;
  int c = memcmp(A->bytes, B->bytes, min);
  if (c != 0) return (c < 0) ? -1 : +1;

  // tie: shorter first
  if (A->len != B->len) return (A->len < B->len) ? -1 : +1;

  // tie: kind
  if (A->kind != B->kind) return (A->kind < B->kind) ? -1 : +1;

  // final: index
  return (a_idx < b_idx) ? -1 : +1;
}

// --------------------
// Graph store (simple vector; deterministic scans)
// --------------------
void canb_graph_init(canb_graph_t* g, canb_edge_t* edges, uint32_t cap) {
  g->edges = edges;
  g->cap = cap;
  g->len = 0;
}

static int edge_eq(const canb_edge_t* e, uint32_t s, uint32_t r, uint32_t d) {
  return e->src == s && e->rel == r && e->dst == d;
}

static canb_err_t graph_add(canb_graph_t* g, uint32_t s, uint32_t r, uint32_t d) {
  // avoid duplicates
  for (uint32_t i = 0; i < g->len; i++) {
    if (edge_eq(&g->edges[i], s, r, d)) return CANB_OK;
  }
  if (g->len >= g->cap) return CANB_ERR_GRAPH_FULL;
  g->edges[g->len++] = (canb_edge_t){ s, r, d };
  return CANB_OK;
}

static void graph_del(canb_graph_t* g, uint32_t s, uint32_t r, uint32_t d) {
  for (uint32_t i = 0; i < g->len; i++) {
    if (edge_eq(&g->edges[i], s, r, d)) {
      // swap-remove (order irrelevant because iteration sorts results by atom bytes)
      g->edges[i] = g->edges[g->len - 1];
      g->len--;
      return;
    }
  }
}

static int graph_has(const canb_graph_t* g, uint32_t s, uint32_t r, uint32_t d) {
  for (uint32_t i = 0; i < g->len; i++) {
    if (edge_eq(&g->edges[i], s, r, d)) return 1;
  }
  return 0;
}

// find minimal dst satisfying (src,rel,?)
// returns CANB_NIL_ATOM if none
static uint32_t graph_find_srd_min(const canb_program_t* P, const canb_graph_t* g, uint32_t s, uint32_t r) {
  uint32_t best = CANB_NIL_ATOM;
  for (uint32_t i = 0; i < g->len; i++) {
    const canb_edge_t* e = &g->edges[i];
    if (e->src == s && e->rel == r) {
      if (best == CANB_NIL_ATOM || canb_atom_cmp(P, e->dst, best) < 0) best = e->dst;
    }
  }
  return best;
}

// find minimal src satisfying (?,rel,dst)
static uint32_t graph_find_rdd_min(const canb_program_t* P, const canb_graph_t* g, uint32_t r, uint32_t d) {
  uint32_t best = CANB_NIL_ATOM;
  for (uint32_t i = 0; i < g->len; i++) {
    const canb_edge_t* e = &g->edges[i];
    if (e->rel == r && e->dst == d) {
      if (best == CANB_NIL_ATOM || canb_atom_cmp(P, e->src, best) < 0) best = e->src;
    }
  }
  return best;
}

// collect up to k dst atoms for (src,rel,?) in stable sorted order.
// embedded-friendly approach: collect into a small local buffer if k is small,
// otherwise do k passes selecting next-min each time (O(k * E)).
static uint32_t graph_iter_srd_push(
  canb_vm_t* vm, const canb_program_t* P,
  uint32_t* sp,
  uint32_t s, uint32_t r, uint32_t k
) {
  uint32_t pushed = 0;
  uint32_t last = CANB_NIL_ATOM;

  while (pushed < k) {
    uint32_t next = CANB_NIL_ATOM;
    for (uint32_t i = 0; i < vm->graph.len; i++) {
      const canb_edge_t* e = &vm->graph.edges[i];
      if (e->src != s || e->rel != r) continue;

      // choose the smallest atom > last
      if (last != CANB_NIL_ATOM && canb_atom_cmp(P, e->dst, last) <= 0) continue;

      if (next == CANB_NIL_ATOM || canb_atom_cmp(P, e->dst, next) < 0) next = e->dst;
    }
    if (next == CANB_NIL_ATOM) break;

    canb_val_t v = { .kind = CANB_VAL_ATOM, .as.atom_index = next };
    if (push(vm, sp, v) != CANB_OK) break;
    pushed++;
    last = next;
  }

  return pushed;
}

// --------------------
// Session helpers
// --------------------
void canb_session_init(canb_session_t* s, uint32_t* fd_to_inode, uint32_t fd_cap) {
  s->fd_to_inode = fd_to_inode;
  s->fd_cap = fd_cap;
  s->fd_next = 0;
  s->cwd_atom = CANB_NIL_ATOM;
  for (uint32_t i = 0; i < fd_cap; i++) s->fd_to_inode[i] = CANB_NIL_ATOM;
}

// fd token is synthetic atom index: base = program.atom_count + fd_number
static canb_err_t fd_alloc(canb_vm_t* vm, const canb_program_t* P, uint32_t inode_atom, uint32_t* out_fd_atom) {
  if (vm->session.fd_next >= vm->session.fd_cap) return CANB_ERR_FD_FULL;
  uint32_t fd = vm->session.fd_next++;
  vm->session.fd_to_inode[fd] = inode_atom;
  *out_fd_atom = P->atom_count + fd;
  return CANB_OK;
}

static uint32_t fd_lookup(canb_vm_t* vm, const canb_program_t* P, uint32_t fd_atom) {
  if (fd_atom < P->atom_count) return CANB_NIL_ATOM; // not synthetic
  uint32_t fd = fd_atom - P->atom_count;
  if (fd >= vm->session.fd_cap) return CANB_NIL_ATOM;
  return vm->session.fd_to_inode[fd];
}

// --------------------
// Type checks
// --------------------
static canb_err_t require_atom(const canb_val_t* v) {
  return (v->kind == CANB_VAL_ATOM) ? CANB_OK : CANB_ERR_TYPE;
}
static canb_err_t require_bool(const canb_val_t* v) {
  return (v->kind == CANB_VAL_BOOL) ? CANB_OK : CANB_ERR_TYPE;
}

// --------------------
// VM run
// --------------------
canb_run_result_t canb_vm_run(canb_vm_t* vm, const canb_program_t* P, uint32_t step_limit) {
  canb_run_result_t R = {0};
  if (!vm || !P) { R.err = CANB_ERR_BAD_CODE; return R; }

  const uint8_t* code = P->code;
  const uint32_t clen = P->code_len;

  uint32_t pc = 0;
  uint32_t sp = 0;
  uint32_t steps = 0;

  while (pc < clen) {
    if (step_limit && steps >= step_limit) {
      R.err = CANB_OK;
      R.pc = pc;
      R.steps = steps;
      return R;
    }

    uint8_t op = code[pc++];

    steps++;

    switch (op) {
      case CANB_OP_NOP: break;

      case CANB_OP_HALT:
        R.err = CANB_OK; R.pc = pc; R.steps = steps; return R;

      case CANB_OP_PUSH_ATOM: {
        uint32_t a = 0;
        canb_err_t e = code_read_uleb32(code, clen, &pc, &a);
        if (e != CANB_OK) { R.err = e; goto done; }
        if (a >= P->atom_count) { R.err = CANB_ERR_BAD_ATOM; goto done; }
        canb_val_t v = { .kind = CANB_VAL_ATOM, .as.atom_index = a };
        R.err = push(vm, &sp, v); if (R.err) goto done;
      } break;

      case CANB_OP_PUSH_BYTES: {
        const uint8_t* p = 0; uint32_t n = 0;
        canb_err_t e = code_read_bytes(code, clen, &pc, &p, &n);
        if (e != CANB_OK) { R.err = e; goto done; }
        canb_val_t v = { .kind = CANB_VAL_BYTES, .as.bytes = { p, n } };
        R.err = push(vm, &sp, v); if (R.err) goto done;
      } break;

      case CANB_OP_PUSH_TRUE: {
        canb_val_t v = { .kind = CANB_VAL_BOOL, .as.b = 1 };
        R.err = push(vm, &sp, v); if (R.err) goto done;
      } break;

      case CANB_OP_PUSH_FALSE: {
        canb_val_t v = { .kind = CANB_VAL_BOOL, .as.b = 0 };
        R.err = push(vm, &sp, v); if (R.err) goto done;
      } break;

      case CANB_OP_DROP: {
        canb_val_t x;
        R.err = pop(vm, &sp, &x); if (R.err) goto done;
      } break;

      case CANB_OP_DUP: {
        canb_val_t x;
        R.err = peek(vm, sp, &x); if (R.err) goto done;
        R.err = push(vm, &sp, x); if (R.err) goto done;
      } break;

      case CANB_OP_SWAP: {
        if (sp < 2) { R.err = CANB_ERR_STACK_UNDERFLOW; goto done; }
        canb_val_t a = vm->stack[sp - 1];
        canb_val_t b = vm->stack[sp - 2];
        vm->stack[sp - 1] = b;
        vm->stack[sp - 2] = a;
      } break;

      case CANB_OP_EQ: {
        canb_val_t a, b;
        R.err = pop(vm, &sp, &a); if (R.err) goto done;
        R.err = pop(vm, &sp, &b); if (R.err) goto done;

        uint8_t eq = 0;
        if (a.kind != b.kind) eq = 0;
        else if (a.kind == CANB_VAL_NIL) eq = 1;
        else if (a.kind == CANB_VAL_BOOL) eq = (a.as.b == b.as.b);
        else if (a.kind == CANB_VAL_ATOM) eq = (a.as.atom_index == b.as.atom_index);
        else if (a.kind == CANB_VAL_BYTES) {
          eq = (a.as.bytes.n == b.as.bytes.n) &&
               (memcmp(a.as.bytes.p, b.as.bytes.p, a.as.bytes.n) == 0);
        }

        canb_val_t v = { .kind = CANB_VAL_BOOL, .as.b = eq };
        R.err = push(vm, &sp, v); if (R.err) goto done;
      } break;

      case CANB_OP_JUMP: {
        uint32_t target = 0;
        canb_err_t e = code_read_uleb32(code, clen, &pc, &target);
        if (e != CANB_OK) { R.err = e; goto done; }
        if (target > clen) { R.err = CANB_ERR_BAD_JUMP; goto done; }
        pc = target;
      } break;

      case CANB_OP_JUMP_IF: {
        uint32_t target = 0;
        canb_err_t e = code_read_uleb32(code, clen, &pc, &target);
        if (e != CANB_OK) { R.err = e; goto done; }
        if (target > clen) { R.err = CANB_ERR_BAD_JUMP; goto done; }

        canb_val_t cond;
        R.err = pop(vm, &sp, &cond); if (R.err) goto done;
        R.err = require_bool(&cond); if (R.err) goto done;
        if (cond.as.b) pc = target;
      } break;

      // -------- Graph ops --------
      case CANB_OP_EDGE_ADD: {
        canb_val_t dst, rel, src;
        R.err = pop(vm, &sp, &dst); if (R.err) goto done;
        R.err = pop(vm, &sp, &rel); if (R.err) goto done;
        R.err = pop(vm, &sp, &src); if (R.err) goto done;

        if ((R.err = require_atom(&src)) ||
            (R.err = require_atom(&rel)) ||
            (R.err = require_atom(&dst))) goto done;

        R.err = graph_add(&vm->graph, src.as.atom_index, rel.as.atom_index, dst.as.atom_index);
        if (R.err) goto done;
      } break;

      case CANB_OP_EDGE_DEL: {
        canb_val_t dst, rel, src;
        R.err = pop(vm, &sp, &dst); if (R.err) goto done;
        R.err = pop(vm, &sp, &rel); if (R.err) goto done;
        R.err = pop(vm, &sp, &src); if (R.err) goto done;

        if ((R.err = require_atom(&src)) ||
            (R.err = require_atom(&rel)) ||
            (R.err = require_atom(&dst))) goto done;

        graph_del(&vm->graph, src.as.atom_index, rel.as.atom_index, dst.as.atom_index);
      } break;

      case CANB_OP_EDGE_HAS: {
        canb_val_t dst, rel, src;
        R.err = pop(vm, &sp, &dst); if (R.err) goto done;
        R.err = pop(vm, &sp, &rel); if (R.err) goto done;
        R.err = pop(vm, &sp, &src); if (R.err) goto done;

        if ((R.err = require_atom(&src)) ||
            (R.err = require_atom(&rel)) ||
            (R.err = require_atom(&dst))) goto done;

        uint8_t has = (uint8_t)graph_has(&vm->graph, src.as.atom_index, rel.as.atom_index, dst.as.atom_index);
        canb_val_t v = { .kind = CANB_VAL_BOOL, .as.b = has };
        R.err = push(vm, &sp, v); if (R.err) goto done;
      } break;

      case CANB_OP_EDGE_FIND_SRD: {
        canb_val_t rel, src;
        R.err = pop(vm, &sp, &src); if (R.err) goto done;
        R.err = pop(vm, &sp, &rel); if (R.err) goto done;

        if ((R.err = require_atom(&src)) || (R.err = require_atom(&rel))) goto done;

        uint32_t dst_atom = graph_find_srd_min(P, &vm->graph, src.as.atom_index, rel.as.atom_index);
        if (dst_atom == CANB_NIL_ATOM) {
          canb_val_t v = { .kind = CANB_VAL_NIL };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        } else {
          canb_val_t v = { .kind = CANB_VAL_ATOM, .as.atom_index = dst_atom };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        }
      } break;

      case CANB_OP_EDGE_FIND_RDD: {
        canb_val_t dst, rel;
        R.err = pop(vm, &sp, &dst); if (R.err) goto done;
        R.err = pop(vm, &sp, &rel); if (R.err) goto done;

        if ((R.err = require_atom(&dst)) || (R.err = require_atom(&rel))) goto done;

        uint32_t src_atom = graph_find_rdd_min(P, &vm->graph, rel.as.atom_index, dst.as.atom_index);
        if (src_atom == CANB_NIL_ATOM) {
          canb_val_t v = { .kind = CANB_VAL_NIL };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        } else {
          canb_val_t v = { .kind = CANB_VAL_ATOM, .as.atom_index = src_atom };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        }
      } break;

      case CANB_OP_EDGE_ITER_SRD: {
        uint32_t k = 0;
        canb_err_t e = code_read_uleb32(code, clen, &pc, &k);
        if (e != CANB_OK) { R.err = e; goto done; }

        canb_val_t rel, src;
        R.err = pop(vm, &sp, &src); if (R.err) goto done;
        R.err = pop(vm, &sp, &rel); if (R.err) goto done;

        if ((R.err = require_atom(&src)) || (R.err = require_atom(&rel))) goto done;

        (void)graph_iter_srd_push(vm, P, &sp, src.as.atom_index, rel.as.atom_index, k);
      } break;

      // -------- Session (FD/CWD) --------
      case CANB_OP_FD_ALLOC: {
        canb_val_t inode;
        R.err = pop(vm, &sp, &inode); if (R.err) goto done;
        if ((R.err = require_atom(&inode))) goto done;

        uint32_t fd_atom = 0;
        R.err = fd_alloc(vm, P, inode.as.atom_index, &fd_atom);
        if (R.err) goto done;

        canb_val_t v = { .kind = CANB_VAL_ATOM, .as.atom_index = fd_atom };
        R.err = push(vm, &sp, v); if (R.err) goto done;
      } break;

      case CANB_OP_FD_LOOKUP: {
        canb_val_t fdv;
        R.err = pop(vm, &sp, &fdv); if (R.err) goto done;
        if ((R.err = require_atom(&fdv))) goto done;

        uint32_t inode_atom = fd_lookup(vm, P, fdv.as.atom_index);
        if (inode_atom == CANB_NIL_ATOM) {
          canb_val_t v = { .kind = CANB_VAL_NIL };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        } else {
          canb_val_t v = { .kind = CANB_VAL_ATOM, .as.atom_index = inode_atom };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        }
      } break;

      case CANB_OP_SET_CWD: {
        canb_val_t dir;
        R.err = pop(vm, &sp, &dir); if (R.err) goto done;
        if ((R.err = require_atom(&dir))) goto done;
        vm->session.cwd_atom = dir.as.atom_index;
      } break;

      case CANB_OP_GET_CWD: {
        if (vm->session.cwd_atom == CANB_NIL_ATOM) {
          canb_val_t v = { .kind = CANB_VAL_NIL };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        } else {
          canb_val_t v = { .kind = CANB_VAL_ATOM, .as.atom_index = vm->session.cwd_atom };
          R.err = push(vm, &sp, v); if (R.err) goto done;
        }
      } break;

      // -------- Trace --------
      case CANB_OP_TRACE: {
        uint32_t a = 0;
        canb_err_t e = code_read_uleb32(code, clen, &pc, &a);
        if (e != CANB_OK) { R.err = e; goto done; }
        if (a >= P->atom_count) { R.err = CANB_ERR_BAD_ATOM; goto done; }
        if (vm->trace_atom) vm->trace_atom(vm->trace_user, P, a);
      } break;

      case CANB_OP_TRACE_TOP: {
        if (vm->trace_val) {
          canb_val_t top;
          R.err = peek(vm, sp, &top); if (R.err) goto done;
          vm->trace_val(vm->trace_user, P, &top);
        }
      } break;

      default:
        R.err = CANB_ERR_BAD_CODE;
        goto done;
    }
  }

  R.err = CANB_OK;

done:
  R.pc = pc;
  R.steps = steps;
  return R;
}
```

---

## Minimal usage snippet (drop into a test harness)

```c
#include "canb.h"
#include "canb_opcodes.h"
#include <stdio.h>

static void tr_atom(void* u, const canb_program_t* P, uint32_t a) {
  (void)u;
  const canb_atom_t* at = &P->atoms[a];
  printf("[TRACE] atom[%u] kind=%u len=%u\n", a, at->kind, at->len);
}

int main(void) {
  // Load a .canb file into memory (left to you)
  // Suppose: uint8_t* buf; size_t len;

  // Decode
  canb_atom_t atoms[256];
  canb_program_t P;
  // canb_err_t e = canb_decode_program(&P, buf, len, atoms, 256);

  // VM storage
  canb_val_t stack[256];
  canb_edge_t edges[1024];
  uint32_t fd_map[32];

  canb_vm_t vm = {0};
  vm.stack = stack;
  vm.stack_cap = 256;
  canb_graph_init(&vm.graph, edges, 1024);
  canb_session_init(&vm.session, fd_map, 32);
  vm.trace_atom = tr_atom;

  // Run
  // canb_run_result_t r = canb_vm_run(&vm, &P, 100000);
  // printf("err=%d pc=%u steps=%u\n", r.err, r.pc, r.steps);

  return 0;
}
```

---

# Notes (important for your philosophy)

* **No numeric indices are “truth”**: atom indices are just compression. Canonical identity is the atom bytes.
* **Graph iteration is deterministic**: `EDGE_FIND_*` chooses minimal by canonical atom byte ordering; `EDGE_ITER_*` emits in strictly increasing order.
* **Session tokens are explicitly non-canonical**: FD atoms are synthetic indices >= `atom_count`. You can later swap this to “push bytes token” instead if you want *zero* synthetic atom indices.