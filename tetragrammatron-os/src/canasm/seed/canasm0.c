/*
  CANASM0 — Seed assembler for CANB v1
  - Input: tokenized lines, space-separated
      PUSH ATOM:hello
      EDGE_ADD
      PROJ_FANO
      HALT
  - Output: CANB container:
      0..3   "CANB"
      4      version (1)
      5..    atom_count (ULEB128)
             atoms: [len ULEB][utf8 bytes]...
             code_len (ULEB)
             code bytes...

  This file is intentionally small and boring. It is the bootstrap seed.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#define CANB_MAGIC "CANB"
#define CANB_VERSION 1

// CANB opcodes (minimal bootstrap set)
enum {
  OP_PUSH_ATOM = 0x01, // uleb atom_index
  OP_EDGE_ADD  = 0x02, // pops b,a; adds edge (a->b)
  OP_PROJ_FANO = 0x03, // (optional) projects top atom to fano residue
  OP_HALT      = 0xFF
};

static void die(const char* msg) {
  fprintf(stderr, "CANASM0 error: %s\n", msg);
  exit(1);
}

static void* xmalloc(size_t n) {
  void* p = malloc(n);
  if (!p) die("out of memory");
  return p;
}

static char* xstrdup(const char* s) {
  size_t n = strlen(s);
  char* p = (char*)xmalloc(n+1);
  memcpy(p, s, n+1);
  return p;
}

typedef struct {
  char** items;
  size_t len;
  size_t cap;
} StrVec;

static void sv_init(StrVec* v) { v->items=NULL; v->len=0; v->cap=0; }

static void sv_push(StrVec* v, const char* s) {
  if (v->len == v->cap) {
    v->cap = v->cap ? v->cap*2 : 16;
    v->items = (char**)realloc(v->items, v->cap * sizeof(char*));
    if (!v->items) die("out of memory realloc");
  }
  v->items[v->len++] = xstrdup(s);
}

static int sv_find(StrVec* v, const char* s) {
  for (size_t i=0;i<v->len;i++) {
    if (strcmp(v->items[i], s) == 0) return (int)i;
  }
  return -1;
}

static int sv_intern(StrVec* v, const char* s) {
  int idx = sv_find(v, s);
  if (idx >= 0) return idx;
  sv_push(v, s);
  return (int)(v->len - 1);
}

typedef struct {
  uint8_t* b;
  size_t len;
  size_t cap;
} ByteBuf;

static void bb_init(ByteBuf* bb) { bb->b=NULL; bb->len=0; bb->cap=0; }

static void bb_put(ByteBuf* bb, uint8_t x) {
  if (bb->len == bb->cap) {
    bb->cap = bb->cap ? bb->cap*2 : 256;
    bb->b = (uint8_t*)realloc(bb->b, bb->cap);
    if (!bb->b) die("out of memory realloc");
  }
  bb->b[bb->len++] = x;
}

static void bb_putn(ByteBuf* bb, const uint8_t* p, size_t n) {
  for (size_t i=0;i<n;i++) bb_put(bb, p[i]);
}

static void bb_put_uleb(ByteBuf* bb, uint32_t v) {
  while (1) {
    uint8_t byte = (uint8_t)(v & 0x7F);
    v >>= 7;
    if (v) byte |= 0x80;
    bb_put(bb, byte);
    if (!v) break;
  }
}

static char* trim(char* s) {
  while (*s==' ' || *s=='\t' || *s=='\r' || *s=='\n') s++;
  size_t n = strlen(s);
  while (n>0 && (s[n-1]==' ' || s[n-1]=='\t' || s[n-1]=='\r' || s[n-1]=='\n')) {
    s[n-1]=0; n--;
  }
  return s;
}

static bool is_comment_or_empty(const char* s) {
  for (const char* p=s; *p; p++) {
    if (*p==' ' || *p=='\t' || *p=='\r' || *p=='\n') continue;
    return (*p=='#' || (*p=='/' && *(p+1)=='/'));
  }
  return true;
}

typedef struct {
  char* op;
  char* arg; // optional
} Instr;

typedef struct {
  Instr* items;
  size_t len;
  size_t cap;
} InstrVec;

static void iv_init(InstrVec* v){ v->items=NULL; v->len=0; v->cap=0; }

static void iv_push(InstrVec* v, const char* op, const char* arg) {
  if (v->len == v->cap) {
    v->cap = v->cap ? v->cap*2 : 128;
    v->items = (Instr*)realloc(v->items, v->cap * sizeof(Instr));
    if (!v->items) die("out of memory realloc");
  }
  v->items[v->len].op = xstrdup(op);
  v->items[v->len].arg = arg ? xstrdup(arg) : NULL;
  v->len++;
}

static void parse_file(FILE* f, StrVec* atoms, InstrVec* instrs) {
  char line[4096];
  while (fgets(line, sizeof(line), f)) {
    char* s = trim(line);
    if (is_comment_or_empty(s)) continue;

    // split by whitespace into at most 2 tokens
    char* op = strtok(s, " \t");
    char* arg = strtok(NULL, " \t");
    if (!op) continue;

    if (strcmp(op, "PUSH") == 0) {
      if (!arg) die("PUSH requires operand like ATOM:name");
      if (strncmp(arg, "ATOM:", 5) != 0) die("PUSH operand must be ATOM:<name>");
      const char* name = arg + 5;
      if (*name == 0) die("ATOM: name empty");
      sv_intern(atoms, name);
      iv_push(instrs, "PUSH", name);
    } else if (strcmp(op, "EDGE_ADD") == 0) {
      iv_push(instrs, "EDGE_ADD", NULL);
    } else if (strcmp(op, "PROJ_FANO") == 0) {
      iv_push(instrs, "PROJ_FANO", NULL);
    } else if (strcmp(op, "HALT") == 0) {
      iv_push(instrs, "HALT", NULL);
    } else {
      fprintf(stderr, "Unknown op: %s\n", op);
      die("unknown opcode (CANASM0 supports PUSH/EDGE_ADD/PROJ_FANO/HALT)");
    }

    // ensure no trailing tokens beyond 2
    char* extra = strtok(NULL, " \t");
    if (extra) die("too many tokens on a line (CANASM0: op [arg])");
  }
}

static void emit_canb(FILE* out, StrVec* atoms, InstrVec* instrs) {
  ByteBuf code; bb_init(&code);

  for (size_t i=0;i<instrs->len;i++) {
    Instr* in = &instrs->items[i];
    if (strcmp(in->op, "PUSH") == 0) {
      int idx = sv_find(atoms, in->arg);
      if (idx < 0) die("internal: atom not interned");
      bb_put(&code, OP_PUSH_ATOM);
      bb_put_uleb(&code, (uint32_t)idx);
    } else if (strcmp(in->op, "EDGE_ADD") == 0) {
      bb_put(&code, OP_EDGE_ADD);
    } else if (strcmp(in->op, "PROJ_FANO") == 0) {
      bb_put(&code, OP_PROJ_FANO);
    } else if (strcmp(in->op, "HALT") == 0) {
      bb_put(&code, OP_HALT);
    } else {
      die("internal: unknown instr in emitter");
    }
  }

  // header
  fwrite(CANB_MAGIC, 1, 4, out);
  fputc((uint8_t)CANB_VERSION, out);

  // atoms
  ByteBuf tmp; bb_init(&tmp);
  bb_put_uleb(&tmp, (uint32_t)atoms->len);
  for (size_t i=0;i<atoms->len;i++) {
    const char* s = atoms->items[i];
    size_t n = strlen(s);
    bb_put_uleb(&tmp, (uint32_t)n);
    bb_putn(&tmp, (const uint8_t*)s, n);
  }

  // code length + bytes
  bb_put_uleb(&tmp, (uint32_t)code.len);
  bb_putn(&tmp, code.b, code.len);

  fwrite(tmp.b, 1, tmp.len, out);
}

static void usage(const char* argv0) {
  fprintf(stderr, "Usage: %s input.canasm -o output.canb\n", argv0);
}

int main(int argc, char** argv) {
  const char* in_path = NULL;
  const char* out_path = NULL;

  for (int i=1;i<argc;i++) {
    if (strcmp(argv[i], "-o") == 0) {
      if (i+1 >= argc) { usage(argv[0]); return 2; }
      out_path = argv[++i];
    } else if (!in_path) {
      in_path = argv[i];
    } else {
      usage(argv[0]);
      return 2;
    }
  }
  if (!in_path || !out_path) { usage(argv[0]); return 2; }

  FILE* f = fopen(in_path, "rb");
  if (!f) { perror("open input"); return 1; }

  StrVec atoms; sv_init(&atoms);
  InstrVec instrs; iv_init(&instrs);
  parse_file(f, &atoms, &instrs);
  fclose(f);

  FILE* out = fopen(out_path, "wb");
  if (!out) { perror("open output"); return 1; }
  emit_canb(out, &atoms, &instrs);
  fclose(out);

  return 0;
}
