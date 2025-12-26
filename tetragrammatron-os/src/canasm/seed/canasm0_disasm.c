// canasm0_disasm.c — CANB v1 disassembler (bootstrap)
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Builds a human-readable .canasm from a .canb file emitted by canasm0.c.
// This matches the CANB v1 format in canasm0.c:
//
//   4 bytes  magic "CANB"
//   1 byte   version (1)
//   uleb     atom_count
//     repeat atom_count:
//       uleb len
//       bytes utf-8 atom string (len bytes, no NUL)
//   uleb     code_len
//   bytes    code (code_len bytes)
//
// Code opcodes (as in canvm_wasm.c):
//   0x01 ATOM uleb(atom_id)
//   0x02 EDGE
//   0x03 PROJ_FANO
//   0xFF HALT
//
// NOTE: This is intentionally small and portable (C99).

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static int read_u8(FILE* f, uint8_t* out) {
  int c = fgetc(f);
  if (c == EOF) return 0;
  *out = (uint8_t)c;
  return 1;
}

static int read_bytes(FILE* f, uint8_t* buf, size_t n) {
  return fread(buf, 1, n, f) == n;
}

static int read_uleb(FILE* f, uint32_t* out) {
  uint32_t result = 0;
  uint32_t shift = 0;
  for (;;) {
    uint8_t byte;
    if (!read_u8(f, &byte)) return 0;
    result |= (uint32_t)(byte & 0x7F) << shift;
    if ((byte & 0x80) == 0) break;
    shift += 7;
    if (shift > 28) return 0; // guard
  }
  *out = result;
  return 1;
}

static void die(const char* msg) {
  fprintf(stderr, "canasm0-disasm: %s\n", msg);
  exit(2);
}

int main(int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s input.canb > output.canasm\n", argv[0]);
    return 2;
  }

  const char* in_path = argv[1];
  FILE* f = fopen(in_path, "rb");
  if (!f) die("failed to open input");

  uint8_t magic[4];
  if (!read_bytes(f, magic, 4)) die("short read (magic)");
  if (memcmp(magic, "CANB", 4) != 0) die("bad magic (expected CANB)");

  uint8_t ver;
  if (!read_u8(f, &ver)) die("short read (version)");
  if (ver != 1) die("unsupported version (expected 1)");

  uint32_t atom_count = 0;
  if (!read_uleb(f, &atom_count)) die("bad atom_count uleb");

  char** atoms = (char**)calloc(atom_count, sizeof(char*));
  if (!atoms) die("oom atoms");

  for (uint32_t i = 0; i < atom_count; i++) {
    uint32_t n = 0;
    if (!read_uleb(f, &n)) die("bad atom len uleb");
    char* s = (char*)malloc((size_t)n + 1);
    if (!s) die("oom atom string");
    if (!read_bytes(f, (uint8_t*)s, n)) die("short read (atom string)");
    s[n] = 0;
    atoms[i] = s;
  }

  uint32_t code_len = 0;
  if (!read_uleb(f, &code_len)) die("bad code_len uleb");
  uint8_t* code = (uint8_t*)malloc(code_len);
  if (!code) die("oom code");
  if (!read_bytes(f, code, code_len)) die("short read (code)");

  // Emit CANASM
  printf("; Disassembled from %s (CANB v1)\n\n", in_path);
  printf("@atoms\n");
  for (uint32_t i = 0; i < atom_count; i++) {
    printf("  %s\n", atoms[i]);
  }
  printf("@end\n\n");

  printf("@code\n");
  // decode bytecode
  size_t pc = 0;
  while (pc < code_len) {
    uint8_t op = code[pc++];

    if (op == 0x01) { // ATOM
      // read uleb from code stream
      uint32_t id = 0;
      uint32_t shift = 0;
      for (;;) {
        if (pc >= code_len) die("truncated uleb in ATOM");
        uint8_t b = code[pc++];
        id |= (uint32_t)(b & 0x7F) << shift;
        if ((b & 0x80) == 0) break;
        shift += 7;
        if (shift > 28) die("uleb too large in ATOM");
      }
      if (id >= atom_count) die("ATOM id out of range");
      printf("  ATOM %s\n", atoms[id]);
      continue;
    }

    if (op == 0x02) { printf("  EDGE\n"); continue; }
    if (op == 0x03) { printf("  PROJ_FANO\n"); continue; }
    if (op == 0xFF) { printf("  HALT\n"); break; }

    // Unknown opcode: show raw byte
    printf("  ; UNKNOWN 0x%02X\n", op);
  }
  printf("@end\n");

  // cleanup
  for (uint32_t i = 0; i < atom_count; i++) free(atoms[i]);
  free(atoms);
  free(code);
  fclose(f);
  return 0;
}
