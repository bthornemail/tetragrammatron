# CANASM / CANB bootstrap slice

## What you get
- **CANASM0 (C)**: a tiny seed assembler that emits a **CANB v1** container.
- **CANVM (WASM C)**: a minimal interpreter + **event ring buffer** for browser projection.
- **Web demo**: loads wasm, runs a sample `.canb`, renders **Fano-line highlight** SVG.

## Build: seed assembler
```bash
cc -O2 -o canasm0 src/canasm/seed/canasm0.c
./canasm0 web/demo/sample.canasm -o web/demo/sample.canb
```

## Build: WASM VM
Example using clang:
```bash
clang --target=wasm32 -O2 -nostdlib \
  -Wl,--no-entry -Wl,--export-all -Wl,--allow-undefined \
  -o web/demo/canvm.wasm src/canb/wasm/canvm_wasm.c
```

## Run demo
```bash
cd web/demo
python3 -m http.server 8000
# open http://localhost:8000
```

## CAN-ISA → CANB bridge (minimal, stable)
Treat **origami/fold** as a *relational program* first, and only later “lower” to specialized opcodes.

**Bridge rule (early vertical slice):**
- `FOLD_AXIOM_k`  ⇒ `PUSH ATOM:axiom:k` + `PUSH ATOM:crease:<id>` + `EDGE_ADD` + `PROJ_FANO`
- `MEET(GCD)`     ⇒ `EDGE_ADD` + `PROJ_FANO` (barrier hook)
- `JOIN(LCM)`     ⇒ `EDGE_ADD` + `PROJ_FANO` (barrier hook)
- `CANON`         ⇒ `PROJ_FANO` (normalize-as-projection hook)

This lets you ship:
1) **assembler** (CANASM0)  
2) **VM** (CANVM)  
3) **visualizer** (Fano projection)  
without freezing the final algebraic VM too early.


## A then B (what you asked for)

### A) Bridge: Origami/Fold → CANB primitives
See:
- `src/canb/spec/CANISA_CANB_BRIDGE.md`

This defines how higher-level fold semantics expand into the minimal **ATOM/EDGE/PROJ_FANO/HALT** core.

### B) Toolchain: assembler + disassembler + first program

Build seed tools:

```bash
make
```

Assemble the single-pass, no-jumps seed:

```bash
./bin/canasm0 src/canasm/seed/canasm1.canasm -o /tmp/demo.canb
```

Disassemble back to text:

```bash
./bin/canasm0-disasm /tmp/demo.canb > /tmp/demo.canasm
```

