# Tetragrammatron Documentation

Welcome to the Tetragrammatron Project documentation. This guide will help you navigate the available documentation resources.

## Documentation Structure

```
docs/
├── README.md                    ← You are here
├── Tetragrammatron-OS.md       ← White paper / Executive summary
├── CONTRIBUTING.md              ← Contribution guidelines
│
├── architecture/                ← System architecture
│   ├── overview.md             ← High-level architecture
│   ├── system-design.md        ← Detailed component design
│   └── projection-model.md     ← Ball/Sphere/Projection theory
│
├── guides/                      ← User guides
│   ├── getting-started.md      ← Installation & first steps
│   └── building-programs.md    ← Writing CANB programs
│
└── reference/                   ← Technical reference
    ├── canb-specification.md   ← CANB format spec (normative)
    └── fano-plane.md           ← Fano plane mathematics
```

## Quick Navigation

### New Users: Start Here

1. **[Getting Started](guides/getting-started.md)**
   - Install and build the toolchain
   - Write your first CANB program
   - Understand basic concepts

2. **[Tetragrammatron-OS White Paper](Tetragrammatron-OS.md)**
   - Project philosophy
   - High-level overview
   - Key concepts

3. **[Building Programs](guides/building-programs.md)**
   - CANASM syntax
   - Pattern library
   - Best practices

### Developers: Core Resources

1. **[Architecture Overview](architecture/overview.md)**
   - System components
   - Data flow
   - Design constraints

2. **[System Design](architecture/system-design.md)**
   - Component details
   - Algorithms
   - Implementation notes

3. **[CANB Specification](reference/canb-specification.md)**
   - Binary format (normative)
   - Instruction set
   - Validation rules

### Researchers: Mathematical Foundations

1. **[Projection Model](architecture/projection-model.md)**
   - Ball/Sphere/Projection theory
   - Philosophical foundations
   - Formal definitions

2. **[Fano Plane Reference](reference/fano-plane.md)**
   - Octonion multiplication
   - Closure properties
   - Applications

3. **[Hardware as Relations (RFC-0016)](../tetragrammatron-os/docs/RFC-0016%20—%20Hardware-as-Relations%20Projection%20Layer%20(Normative).md)**
   - Hardware abstraction
   - Capability lattice

### Contributors

1. **[CONTRIBUTING.md](CONTRIBUTING.md)** (coming soon)
   - How to contribute
   - Code standards
   - Development workflow

2. **[System Design](architecture/system-design.md)**
   - Extension points
   - Testing strategy
   - Performance notes

## By Topic

### CANB Bytecode

| Document | Topic |
|----------|-------|
| [CANB Specification](reference/canb-specification.md) | Binary format, opcodes |
| [Building Programs](guides/building-programs.md) | Writing CANASM source |
| [Getting Started](guides/getting-started.md) | First programs |

### Genesis Protocol

| Document | Topic |
|----------|-------|
| [Getting Started](guides/getting-started.md) | Genesis fingerprinting basics |
| [Integration Guide](../integration/README.md) | Genesis + Tetragrammatron |
| [System Design](architecture/system-design.md) | TPPM algorithm |

### Fano-Gate Validation

| Document | Topic |
|----------|-------|
| [Fano Plane Reference](reference/fano-plane.md) | Mathematical foundation |
| [Building Programs](guides/building-programs.md) | Valid atom subsets |
| [Integration Guide](../integration/README.md) | Validator usage |

### Projections

| Document | Topic |
|----------|-------|
| [Projection Model](architecture/projection-model.md) | Theory and design patterns |
| [System Design](architecture/system-design.md) | Implementation |
| [Getting Started](guides/getting-started.md) | SVG/GLB demos |

### Architecture & Philosophy

| Document | Topic |
|----------|-------|
| [Tetragrammatron-OS White Paper](Tetragrammatron-OS.md) | Executive summary |
| [Architecture Overview](architecture/overview.md) | System structure |
| [Projection Model](architecture/projection-model.md) | Ball/Sphere theory |

## Subsystem Documentation

Additional documentation is located in subsystem directories:

### Tetragrammatron-OS

```
tetragrammatron-os/docs/
├── RFC-0016 — Hardware-as-Relations.md
├── RFC-0017 — POSIX-as-Projection.md
└── ... (implementation RFCs)
```

**See**: [tetragrammatron-os/docs/](../tetragrammatron-os/docs/)

### Genesis Protocol

```
genesis-protocol/
├── GENESIS.org          ← Canonical declaration
├── README.org           ← Public orientation
└── GLOSSARY.org         ← Language contract
```

**See**: [genesis-protocol/](../genesis-protocol/)

### Revelation Observer

```
revelation-observer/
├── README.md            ← Complete guide
├── bin/
│   └── genesis-diffuse.sh  ← 8-pass diffusion
├── examples/
│   ├── demo-comparison.sh  ← Structural comparison
│   └── demo-evolution.sh   ← Evolution tracking
└── REVELATION_COMPLETE.md  ← Implementation summary
```

**See**: [revelation-observer/README.md](../revelation-observer/README.md)

### Integration

```
integration/
├── README.md            ← Integration guide
└── FANO_VALIDATOR_COMPLETE.md  ← Validator documentation
```

**See**: [integration/README.md](../integration/README.md)

## Document Status

| Document | Status | Last Updated |
|----------|--------|--------------|
| [Tetragrammatron-OS.md](Tetragrammatron-OS.md) | Stable | 2025-12-25 |
| [Architecture Overview](architecture/overview.md) | Stable | 2025-12-25 |
| [System Design](architecture/system-design.md) | Stable | 2025-12-25 |
| [Projection Model](architecture/projection-model.md) | Stable | 2025-12-25 |
| [Getting Started](guides/getting-started.md) | Stable | 2025-12-25 |
| [Building Programs](guides/building-programs.md) | Stable | 2025-12-25 |
| [CANB Specification](reference/canb-specification.md) | Normative | 2025-12-25 |
| [Fano Plane](reference/fano-plane.md) | Stable | 2025-12-25 |

## Finding What You Need

### I want to...

**...get started quickly**
→ [Getting Started Guide](guides/getting-started.md)

**...understand the philosophy**
→ [White Paper](Tetragrammatron-OS.md) + [Projection Model](architecture/projection-model.md)

**...write CANB programs**
→ [Building Programs Guide](guides/building-programs.md)

**...understand the binary format**
→ [CANB Specification](reference/canb-specification.md)

**...learn about Fano validation**
→ [Fano Plane Reference](reference/fano-plane.md) + [Integration Guide](../integration/README.md)

**...understand the architecture**
→ [Architecture Overview](architecture/overview.md) + [System Design](architecture/system-design.md)

**...use structural diffusion (Revelation Observer)**
→ [Revelation Observer Guide](../revelation-observer/README.md)

**...compare file structures**
→ [Revelation Observer](../revelation-observer/README.md) + [Examples](../revelation-observer/examples/)

**...contribute to the project**
→ [CONTRIBUTING.md](CONTRIBUTING.md) (coming soon)

**...use Genesis fingerprinting**
→ [Getting Started](guides/getting-started.md) + [Integration Guide](../integration/README.md)

**...visualize Fano planes**
→ [Getting Started](guides/getting-started.md) (Web Demos section)

### By Experience Level

**Beginner** (never used Tetragrammatron):
1. [Getting Started](guides/getting-started.md)
2. [Tetragrammatron-OS White Paper](Tetragrammatron-OS.md)
3. [Building Programs](guides/building-programs.md)

**Intermediate** (written CANB programs):
1. [Architecture Overview](architecture/overview.md)
2. [Fano Plane Reference](reference/fano-plane.md)
3. [System Design](architecture/system-design.md)

**Advanced** (contributing or extending):
1. [CANB Specification](reference/canb-specification.md)
2. [Projection Model](architecture/projection-model.md)
3. [System Design](architecture/system-design.md)
4. Implementation RFCs in `tetragrammatron-os/docs/`

### By Role

**User** (using the toolchain):
- [Getting Started](guides/getting-started.md)
- [Building Programs](guides/building-programs.md)
- [Integration Guide](../integration/README.md)

**Developer** (implementing features):
- [Architecture Overview](architecture/overview.md)
- [System Design](architecture/system-design.md)
- [CANB Specification](reference/canb-specification.md)

**Researcher** (exploring foundations):
- [Projection Model](architecture/projection-model.md)
- [Fano Plane Reference](reference/fano-plane.md)
- [White Paper](Tetragrammatron-OS.md)

**Integrator** (building on top):
- [CANB Specification](reference/canb-specification.md)
- [System Design](architecture/system-design.md)
- [Integration Guide](../integration/README.md)

## Conventions

### Document Types

**Normative** (binding specification):
- [CANB Specification](reference/canb-specification.md)
- RFCs in `tetragrammatron-os/docs/`

**Stable** (unlikely to change):
- Architecture documents
- Reference documents

**Guide** (instructional):
- Getting Started
- Building Programs

**Informative** (explanatory):
- White Paper
- Design documents

### Code Examples

Code blocks are labeled with language:

```canasm
// CANASM assembly
PUSH ATOM:hello
HALT
```

```bash
# Shell commands
./bin/canasm0 input.canasm -o output.canb
```

```c
/* C code */
void example() { }
```

### Cross-References

- **Internal**: `[Link Text](file.md)` or `[Link Text](../dir/file.md)`
- **External**: `[Link Text](https://example.com)`
- **Code references**: `file.c:123` (file path + line number)

## External Resources

### Official

- **GitHub Repository**: github.com/yourorg/tetragrammatron (link TBD)
- **Issue Tracker**: github.com/yourorg/tetragrammatron/issues
- **Releases**: github.com/yourorg/tetragrammatron/releases

### Community

- **Discussions**: (TBD)
- **Wiki**: (TBD)
- **Chat**: (TBD)

### Related Projects

- **Genesis Protocol**: See `genesis-protocol/` directory
- **Formal Verification**: See `genesis-protocol/formal/`

## Contributing to Docs

### Reporting Issues

Found a typo, broken link, or unclear explanation?

1. Check if already reported
2. Open issue on GitHub
3. Tag with `documentation`

### Suggesting Improvements

Have an idea for better docs?

1. Open issue describing improvement
2. Discuss with maintainers
3. Submit pull request

### Writing New Docs

Want to add documentation?

1. Follow existing structure
2. Use clear, concise language
3. Include code examples
4. Test all examples
5. Update this README

## Version History

### v1.0 (2025-12-25)

**Initial documentation release**:
- Architecture documents (3)
- User guides (2)
- Reference docs (2)
- White paper

**Coverage**:
- ✅ Getting started
- ✅ CANB programming
- ✅ Architecture
- ✅ Fano validation
- ✅ Genesis fingerprinting
- 🔨 Advanced guides (planned)
- 🔨 API documentation (planned)
- 🔨 Tutorials (planned)

## Roadmap

### Planned Documentation

**Guides**:
- Genesis Protocol deep dive
- Fano-gate validation guide
- Web demo tutorial
- Distributed sync howto

**Reference**:
- API documentation (C, JavaScript)
- Command-line reference
- Configuration guide
- Performance benchmarks

**Architecture**:
- Data structures reference
- Concurrency model
- Security model
- Platform porting guide

**Examples**:
- Example programs repository
- Integration patterns
- Common use cases

## Feedback

Documentation is a work in progress. We value your feedback!

**How to provide feedback**:
- Open GitHub issue (tag: `documentation`)
- Suggest edits via pull request
- Join community discussions

**What helps**:
- Specific examples of confusion
- Screenshots of errors
- Suggestions for missing topics
- Links to helpful external resources

---

**Last updated**: 2025-12-25

**Maintainer**: Brian Thorne

**License**: (See LICENSE file in project root)
