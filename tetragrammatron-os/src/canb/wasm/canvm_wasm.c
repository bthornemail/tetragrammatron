/*
  canvm_wasm.c — Minimal CANB v1 VM for WebAssembly projection
  - Loads CANB container
  - Executes small opcode set
  - Emits events into a tiny ring buffer
*/
#include <stdint.h>
#include <stddef.h>

#define CANB_VERSION 1

enum {
  OP_PUSH_ATOM = 0x01,
  OP_EDGE_ADD  = 0x02,
  OP_PROJ_FANO = 0x03,
  OP_HALT      = 0xFF
};

enum {
  EV_PUSH_ATOM = 1,
  EV_EDGE_ADD  = 2,
  EV_PROJ_FANO = 3,
  EV_HALT      = 255,
  EV_FAULT     = 254
};

typedef struct {
  uint8_t tag;
  uint32_t a, b, c;
} Event;

#define EVENT_CAP 256
static Event g_events[EVENT_CAP];
static uint32_t g_ev_w = 0;
static uint32_t g_ev_r = 0;

static void emit(uint8_t tag, uint32_t a, uint32_t b, uint32_t c) {
  uint32_t next = (g_ev_w + 1) % EVENT_CAP;
  if (next == g_ev_r) g_ev_r = (g_ev_r + 1) % EVENT_CAP; // drop oldest
  g_events[g_ev_w].tag = tag;
  g_events[g_ev_w].a = a;
  g_events[g_ev_w].b = b;
  g_events[g_ev_w].c = c;
  g_ev_w = next;
}

static uint32_t read_uleb(const uint8_t* buf, uint32_t len, uint32_t* io) {
  uint32_t x = 0, shift = 0;
  while (*io < len) {
    uint8_t b = buf[(*io)++];
    x |= (uint32_t)(b & 0x7F) << shift;
    if (!(b & 0x80)) return x;
    shift += 7;
    if (shift > 28) break;
  }
  return 0xFFFFFFFFu;
}

typedef struct {
  const uint8_t* blob;
  uint32_t blob_len;

  uint32_t atom_count;
  const uint8_t* code;
  uint32_t code_len;

  uint32_t ip;
  uint32_t halted;

  uint32_t sp;
  uint32_t stack[256];

  uint32_t edge_n;
  uint32_t edges[512];
} VM;

static VM g_vm;

uint32_t can_init(void) {
  for (uint32_t i=0;i<EVENT_CAP;i++) g_events[i].tag = 0;
  g_ev_w = g_ev_r = 0;
  g_vm.blob = 0; g_vm.blob_len = 0;
  g_vm.atom_count = 0; g_vm.code = 0; g_vm.code_len = 0;
  g_vm.ip = 0; g_vm.halted = 0; g_vm.sp = 0; g_vm.edge_n = 0;
  return 1;
}

uint32_t can_load(uint32_t handle, const uint8_t* blob, uint32_t blob_len) {
  (void)handle;
  if (blob_len < 5) { emit(EV_FAULT, 1, 0, 0); return 0; }
  if (!(blob[0]=='C' && blob[1]=='A' && blob[2]=='N' && blob[3]=='B')) { emit(EV_FAULT, 2, 0, 0); return 0; }
  if (blob[4] != CANB_VERSION) { emit(EV_FAULT, 3, blob[4], 0); return 0; }

  uint32_t off = 5;
  uint32_t atom_count = read_uleb(blob, blob_len, &off);
  if (atom_count == 0xFFFFFFFFu) { emit(EV_FAULT, 4, 0, 0); return 0; }

  for (uint32_t i=0;i<atom_count;i++) {
    uint32_t n = read_uleb(blob, blob_len, &off);
    if (n == 0xFFFFFFFFu || off + n > blob_len) { emit(EV_FAULT, 5, i, 0); return 0; }
    off += n;
  }

  uint32_t code_len = read_uleb(blob, blob_len, &off);
  if (code_len == 0xFFFFFFFFu || off + code_len > blob_len) { emit(EV_FAULT, 6, 0, 0); return 0; }

  g_vm.blob = blob; g_vm.blob_len = blob_len;
  g_vm.atom_count = atom_count;
  g_vm.code = blob + off; g_vm.code_len = code_len;
  g_vm.ip = 0; g_vm.halted = 0; g_vm.sp = 0; g_vm.edge_n = 0;
  return 1;
}

static void push(uint32_t v) {
  if (g_vm.sp >= 256) { emit(EV_FAULT, 10, 0, 0); g_vm.halted = 1; return; }
  g_vm.stack[g_vm.sp++] = v;
}
static uint32_t pop(void) {
  if (g_vm.sp == 0) { emit(EV_FAULT, 11, 0, 0); g_vm.halted = 1; return 0; }
  return g_vm.stack[--g_vm.sp];
}

uint32_t can_step(uint32_t handle, uint32_t max_steps) {
  (void)handle;
  if (!g_vm.code || g_vm.halted) return 0;
  uint32_t steps = 0;
  while (steps < max_steps && g_vm.ip < g_vm.code_len && !g_vm.halted) {
    uint8_t op = g_vm.code[g_vm.ip++];
    if (op == OP_PUSH_ATOM) {
      uint32_t idx = read_uleb(g_vm.code, g_vm.code_len, &g_vm.ip);
      if (idx == 0xFFFFFFFFu || idx >= g_vm.atom_count) { emit(EV_FAULT, 20, idx, 0); g_vm.halted=1; break; }
      push(idx);
      emit(EV_PUSH_ATOM, idx, 0, 0);
    } else if (op == OP_EDGE_ADD) {
      uint32_t b = pop(), a = pop();
      if (g_vm.edge_n < 256) {
        g_vm.edges[g_vm.edge_n*2+0] = a;
        g_vm.edges[g_vm.edge_n*2+1] = b;
        g_vm.edge_n++;
      }
      emit(EV_EDGE_ADD, a, b, 0);
    } else if (op == OP_PROJ_FANO) {
      if (g_vm.sp == 0) { emit(EV_FAULT, 21, 0, 0); g_vm.halted=1; break; }
      uint32_t x = g_vm.stack[g_vm.sp-1];
      uint32_t r = (g_vm.atom_count ? (x % 7u) : 0u);
      g_vm.stack[g_vm.sp-1] = r;
      emit(EV_PROJ_FANO, x, r, 0);
    } else if (op == OP_HALT) {
      g_vm.halted = 1;
      emit(EV_HALT, 0, 0, 0);
    } else {
      emit(EV_FAULT, 22, op, g_vm.ip);
      g_vm.halted = 1;
    }
    steps++;
  }
  return steps;
}

uint32_t can_drain_events(uint32_t handle, Event* out_ptr, uint32_t cap) {
  (void)handle;
  uint32_t n = 0;
  while (n < cap && g_ev_r != g_ev_w) {
    out_ptr[n++] = g_events[g_ev_r];
    g_ev_r = (g_ev_r + 1) % EVENT_CAP;
  }
  return n;
}
