# CANB Specification v1

## Overview

**CANB** (Canonical Atom-Node Bytecode) is a platform-independent binary container format for graph-based programs.

**Version**: 1
**Status**: Stable
**Endianness**: Little-endian (for multi-byte integers)
**Character encoding**: UTF-8 (for atom strings)

## File Format

### Container Structure

```
┌──────────────────┐
│ Header           │ 5 bytes (magic + version)
├──────────────────┤
│ Atom Table       │ Variable length
├──────────────────┤
│ Code Section     │ Variable length
└──────────────────┘
```

### Header (5 bytes)

| Offset | Type | Value | Description |
|--------|------|-------|-------------|
| 0-3 | byte[4] | `0x43 0x41 0x4E 0x42` | Magic bytes ("CANB" in ASCII) |
| 4 | uint8 | `0x01` | Format version (currently 1) |

### Atom Table

| Field | Type | Description |
|-------|------|-------------|
| atom_count | ULEB128 | Number of atoms in table |
| atoms | Atom[atom_count] | Array of atom entries |

**Atom entry**:

| Field | Type | Description |
|-------|------|-------------|
| length | ULEB128 | Length of atom name in bytes |
| name | byte[length] | UTF-8 encoded atom name |

**Constraints**:
- Atom names MUST be valid UTF-8
- Atom names SHOULD be printable ASCII for portability
- Atom names MUST NOT contain NUL bytes
- Maximum atom name length: 2^28 - 1 bytes (ULEB128 limit)
- Maximum atom count: 2^28 - 1 (ULEB128 limit)

### Code Section

| Field | Type | Description |
|-------|------|-------------|
| code_length | ULEB128 | Length of code in bytes |
| code | byte[code_length] | Bytecode instructions |

## Data Types

### ULEB128

**Unsigned Little Endian Base 128** encoding (variable-length integer).

**Format**:
- Each byte encodes 7 bits of value
- High bit (0x80) indicates continuation
- Values are reconstructed in little-endian order

**Encoding algorithm**:
```c
void encode_uleb128(uint32_t value) {
  do {
    uint8_t byte = value & 0x7F;
    value >>= 7;
    if (value != 0) {
      byte |= 0x80;  // Set continuation bit
    }
    emit(byte);
  } while (value != 0);
}
```

**Decoding algorithm**:
```c
uint32_t decode_uleb128(FILE* f) {
  uint32_t result = 0;
  uint32_t shift = 0;

  while (1) {
    uint8_t byte = read_byte(f);
    result |= (byte & 0x7F) << shift;

    if ((byte & 0x80) == 0) {
      break;  // No continuation
    }

    shift += 7;
    if (shift > 28) {
      error("ULEB128 too large");
    }
  }

  return result;
}
```

**Examples**:

| Value | Encoding (hex) | Bytes |
|-------|----------------|-------|
| 0 | `00` | 1 |
| 127 | `7F` | 1 |
| 128 | `80 01` | 2 |
| 255 | `FF 01` | 2 |
| 256 | `80 02` | 2 |
| 16383 | `FF 7F` | 2 |
| 16384 | `80 80 01` | 3 |

**Maximum value**: 2^28 - 1 (4-byte ULEB128 encoding, 28 bits of data)

## Instruction Set

### Opcode Table

| Opcode | Name | Arguments | Description |
|--------|------|-----------|-------------|
| 0x01 | PUSH_ATOM | ULEB128(atom_id) | Push atom onto stack |
| 0x02 | EDGE_ADD | - | Pop two atoms, add edge |
| 0x03 | PROJ_FANO | - | Project top atom to Fano plane |
| 0xFF | HALT | - | Terminate execution |

**Reserved opcodes**:
- 0x00: Reserved (invalid)
- 0x04-0xFE: Reserved for future use

### PUSH_ATOM (0x01)

**Description**: Push atom onto stack

**Encoding**:
```
0x01 <atom_id:ULEB128>
```

**Semantics**:
```
stack.push(atoms[atom_id])
```

**Constraints**:
- `atom_id` MUST be < `atom_count`
- Stack MUST NOT overflow (implementation-defined limit)

**Example**:
```
0x01 0x00        # PUSH_ATOM 0
0x01 0x05        # PUSH_ATOM 5
0x01 0x80 0x01   # PUSH_ATOM 128
```

### EDGE_ADD (0x02)

**Description**: Pop two atoms from stack, create directed edge

**Encoding**:
```
0x02
```

**Semantics**:
```
dst = stack.pop()
src = stack.pop()
graph.add_edge(src, dst)
```

**Constraints**:
- Stack MUST contain at least 2 atoms
- Stack underflow is a fatal error

**Example**:
```
# Create edge: A → B
PUSH_ATOM A
PUSH_ATOM B
EDGE_ADD        # Edge: A → B
```

### PROJ_FANO (0x03)

**Description**: Project top stack atom to Fano plane coordinates

**Encoding**:
```
0x03
```

**Semantics**:
```
atom = stack.peek()  # Don't pop
emit_projection(atom, fano_coordinates(atom))
```

**Constraints**:
- Stack MUST contain at least 1 atom
- Implementation MAY output projection to stdout/file

**Use cases**:
- Visualization (SVG, GLB)
- Debugging (show atom positions)
- Validation (check Fano membership)

### HALT (0xFF)

**Description**: Terminate program execution

**Encoding**:
```
0xFF
```

**Semantics**:
```
exit(0)
```

**Constraints**:
- Execution stops immediately
- Stack state is discarded
- Graph state is final

## Execution Model

### Virtual Machine State

```c
typedef struct {
  // Atom table (from CANB container)
  char** atoms;
  size_t atom_count;

  // Execution state
  Atom* stack;       // Stack of atoms
  size_t stack_pointer;

  // Graph store
  Graph* graph;

  // Code
  uint8_t* code;
  size_t code_length;
  size_t program_counter;
} VM;
```

### Execution Loop

```c
void execute(VM* vm) {
  while (vm->pc < vm->code_length) {
    uint8_t opcode = vm->code[vm->pc++];

    switch (opcode) {
      case 0x01: {  // PUSH_ATOM
        uint32_t id = read_uleb128(&vm->pc);
        if (id >= vm->atom_count) {
          fatal("atom_id out of range");
        }
        if (vm->sp >= STACK_MAX) {
          fatal("stack overflow");
        }
        vm->stack[vm->sp++] = vm->atoms[id];
        break;
      }

      case 0x02: {  // EDGE_ADD
        if (vm->sp < 2) {
          fatal("stack underflow");
        }
        Atom dst = vm->stack[--vm->sp];
        Atom src = vm->stack[--vm->sp];
        graph_add_edge(vm->graph, src, dst);
        break;
      }

      case 0x03: {  // PROJ_FANO
        if (vm->sp < 1) {
          fatal("stack underflow");
        }
        Atom atom = vm->stack[vm->sp - 1];  // Peek
        project_fano(atom);
        break;
      }

      case 0xFF: {  // HALT
        return;
      }

      default:
        fatal("invalid opcode");
    }
  }

  // Implicit HALT at end of code
}
```

### Error Handling

**Fatal errors** (execution must stop):
- Invalid opcode
- Stack overflow/underflow
- Atom ID out of range
- Malformed ULEB128

**Non-fatal errors** (implementation-defined):
- Duplicate edges (may be ignored or deduplicated)
- Empty stack on PROJ_FANO (may be no-op)

## Validation Rules

### Structural Validation

**MUST reject**:
- Invalid magic bytes
- Unsupported version
- Truncated file (incomplete ULEB128 or strings)
- Atom names with NUL bytes

**SHOULD warn**:
- Non-ASCII atom names
- Very large atom counts (> 1000)
- Very large code sections (> 1 MB)

### Semantic Validation

**MUST reject** (if Fano-gate enabled):
- Atom sets that violate Fano closure

**MAY reject**:
- Programs with cycles (if DAG required)
- Programs with disconnected components

## Compatibility

### Version 1 Guarantees

1. **Forward compatibility**: v1 parsers MUST reject v2+ files
2. **Backward compatibility**: v2+ parsers SHOULD support v1 files
3. **Endianness**: ULEB128 is endian-independent
4. **Character encoding**: UTF-8 is portable
5. **Reserved opcodes**: Future versions MAY use 0x04-0xFE

### Platform Requirements

**Minimal requirements**:
- 8-bit bytes (CHAR_BIT == 8)
- Support for 32-bit unsigned integers
- UTF-8 string handling

**No requirements for**:
- Specific endianness (ULEB128 is portable)
- Specific word size (32-bit or 64-bit)
- Floating-point support
- SIMD instructions

## Examples

### Example 1: Minimal Program

**Source**:
```canasm
PUSH ATOM:hello
HALT
```

**Binary**:
```
Offset  Bytes                Description
------  -----                -----------
0x00    43 41 4E 42         Magic "CANB"
0x04    01                  Version 1
0x05    01                  Atom count = 1
0x06    05                  Atom 0 length = 5
0x07    68 65 6C 6C 6F      Atom 0 = "hello"
0x0C    02                  Code length = 2
0x0D    01                  PUSH_ATOM
0x0E    00                  atom_id = 0
0x0F    FF                  HALT
```

**Total size**: 16 bytes

### Example 2: Two Atoms, One Edge

**Source**:
```canasm
PUSH ATOM:A
PUSH ATOM:B
EDGE_ADD
HALT
```

**Binary**:
```
Offset  Bytes                Description
------  -----                -----------
0x00    43 41 4E 42         Magic "CANB"
0x04    01                  Version 1
0x05    02                  Atom count = 2
0x06    01                  Atom 0 length = 1
0x07    41                  Atom 0 = "A"
0x08    01                  Atom 1 length = 1
0x09    42                  Atom 1 = "B"
0x0A    05                  Code length = 5
0x0B    01 00               PUSH_ATOM 0
0x0D    01 01               PUSH_ATOM 1
0x0F    02                  EDGE_ADD
0x10    FF                  HALT
```

**Total size**: 17 bytes

### Example 3: Large Atom ID

**Source**:
```canasm
PUSH ATOM:atom0
...
PUSH ATOM:atom200
PUSH ATOM:atom201
EDGE_ADD
HALT
```

**Relevant binary** (showing ULEB128 encoding):
```
...
01 C8 01            # PUSH_ATOM 200 (0xC8 = 0b11001000, 0x01 = 0b00000001)
                    # Value = (0x48) | (0x01 << 7) = 72 + 128 = 200
01 C9 01            # PUSH_ATOM 201
02                  # EDGE_ADD
FF                  # HALT
```

## Implementation Notes

### Atom Interning

Assemblers SHOULD intern atom strings (deduplicate):

```c
// Input:
PUSH ATOM:foo
PUSH ATOM:bar
PUSH ATOM:foo  // Duplicate

// Atom table (interned):
0: "foo"
1: "bar"

// Code:
PUSH_ATOM 0  # foo
PUSH_ATOM 1  # bar
PUSH_ATOM 0  # foo (reused)
```

### Deterministic Assembly

To ensure deterministic bytecode generation:

1. **Atom ordering**: Use first-use order OR sort alphabetically
2. **ULEB128 encoding**: Always use shortest representation
3. **No padding**: Don't insert alignment bytes
4. **UTF-8 normalization**: Use NFC normalization for atom names

### Memory Constraints

**Recommended limits** for embedded systems:

| Resource | Limit | Rationale |
|----------|-------|-----------|
| Atom count | 256 | Fits in 1-byte atom_id (optimization) |
| Stack depth | 256 | Reasonable for most programs |
| Code size | 64 KB | Typical for embedded firmware |
| Atom name length | 64 bytes | Reasonable identifier length |

## Security Considerations

### Denial of Service

**Attack**: Extremely large ULEB128 values

**Mitigation**: Enforce maximum ULEB128 value (2^28 - 1)

**Attack**: Infinite loop (cycle in code)

**Mitigation**: Instruction count limit (implementation-defined)

### Resource Exhaustion

**Attack**: Excessive atom count or code size

**Mitigation**: Enforce reasonable limits before allocation

**Attack**: Stack overflow via many PUSH_ATOM

**Mitigation**: Check stack depth before each push

### Malformed Input

**Attack**: Truncated file, invalid UTF-8

**Mitigation**: Validate header, ULEB128, and UTF-8 before execution

## Future Extensions

### Potential Opcode Additions

- `DUP` (0x04): Duplicate top stack item
- `SWAP` (0x05): Swap top two stack items
- `POP` (0x06): Discard top stack item
- `EDGE_REMOVE` (0x07): Remove edge from graph
- `PROJ_CUSTOM` (0x08): Custom projection with type ID

### Version 2 Considerations

- Extended opcode space (0x00-0x10 reserved for core)
- Type annotations for atoms
- Multiple graph stores
- Edge labels (first-class)

## Appendix A: Grammar (EBNF)

```ebnf
canb_file = header atom_table code_section ;

header = magic version ;
magic = 0x43 0x41 0x4E 0x42 ;  (* "CANB" *)
version = 0x01 ;

atom_table = atom_count atom* ;
atom_count = uleb128 ;
atom = atom_length atom_name ;
atom_length = uleb128 ;
atom_name = utf8_byte+ ;

code_section = code_length instruction* ;
code_length = uleb128 ;
instruction = push_atom | edge_add | proj_fano | halt ;

push_atom = 0x01 uleb128 ;
edge_add = 0x02 ;
proj_fano = 0x03 ;
halt = 0xFF ;

uleb128 = uleb_byte+ ;
uleb_byte = continuation_byte | final_byte ;
continuation_byte = [0x80-0xFF] ;
final_byte = [0x00-0x7F] ;
utf8_byte = [0x00-0xFF] ;  (* Valid UTF-8 sequence *)
```

## Appendix B: Reference Implementation

See:
- `tetragrammatron-os/src/canasm/seed/canasm0.c` - Assembler
- `tetragrammatron-os/src/canasm/seed/canasm0_disasm.c` - Disassembler
- `tetragrammatron-os/src/canb/wasm/canvm_wasm.c` - WebAssembly VM

## Changelog

### Version 1 (2025-12-25)

- Initial specification
- Opcodes: PUSH_ATOM, EDGE_ADD, PROJ_FANO, HALT
- ULEB128 encoding for variable-length integers
- UTF-8 atom names

---

**Status**: Normative
**Last updated**: 2025-12-25
