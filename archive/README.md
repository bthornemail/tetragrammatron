# Tetragrammatron Project

A relational, proof-preserving computational substrate combining **Tetragrammatron-OS** (computation without numbers) and **Genesis Protocol** (coordination without authority).

## Quick Start

```bash
# Build the CANB toolchain
cd tetragrammatron-os/
make

# Test with sample program
./bin/canasm0 src/canasm/seed/canasm1.canasm -o ~/demo.canb
./bin/canasm0-disasm ~/demo.canb

# Generate structural fingerprints
cd ../genesis-protocol/
./genesis.sh translate ~/demo.canb

# Diffuse to structural basis (Revelation Observer)
cd ../revelation-observer/
./bin/genesis-diffuse.sh ~/demo.canb

# Run web demo
cd ../tetragrammatron-os/web/demo/
python3 -m http.server 8000
# Open http://localhost:8000
```

## Project Structure

- **[tetragrammatron-os/](tetragrammatron-os/)** - Core OS implementation (CANB bytecode, VM, assembler)
- **[genesis-protocol/](genesis-protocol/)** - Distributed repository system (structural provenance)
- **[revelation-observer/](revelation-observer/)** - 8-pass structural diffusion (observation without collapse)
- **[integration/](integration/)** - Combined Genesis + Tetragrammatron features
- **[docs/](docs/)** - Project-wide documentation and white papers
- **[tools/](tools/)** - Utility scripts for build, test, deployment
- **[dev/](dev/)** - Development artifacts, archives, research notes

## Key Concepts

### Tetragrammatron-OS
- **Relations precede numbers** - Computation based on graph algebra, not numeric addresses
- **Hardware as Ball, Semantics as Sphere** - Physical constraints separated from invariant logic
- **Fano plane closure** - 7-point octonionic multiplication table as validation gate
- **Projections not primitives** - POSIX, hardware IDs, timestamps are derived views

### Genesis Protocol
- **Structure before interpretation** - Files identified by shape (P₀/P₁ alternation), not content
- **Customizable symbols** - Use α/Ω, e₀/∑, or any symbols for semantic mass/structure
- **8-dimensional fingerprints** - Hardware (8 bytes) + Topology (8 integers) + Form skeleton
- **Distributed sync** - Peer-to-peer atoms exchange without Git or central server

### Revelation Observer
- **Observation without collapse** - View structural emergence without asserting meaning
- **8-pass diffusion** - Regex-only transformations reveal invariants
- **Deterministic basis** - Same structure → same fingerprint (SHA256)
- **No semantic interpretation** - Structure revealed, not meaning imposed

### Integration
- **Fano-gate validation** - Reject CANB programs that violate octonionic closure
- **Provenance chains** - Every CANB file wrapped with cryptographic Genesis metadata
- **GLB export** - 3D Merkaba visualization with provenance timeline
- **Multi-platform** - Works on Linux, Android/Termux, ESP32, browser (WASM)

## Documentation

- **[docs/Tetragrammatron-OS.md](docs/Tetragrammatron-OS.md)** - White paper / executive summary
- **[docs/README.md](docs/README.md)** - Documentation navigation guide
- **[docs/guides/getting-started.md](docs/guides/getting-started.md)** - Installation and first steps
- **[tetragrammatron-os/README.md](tetragrammatron-os/README.md)** - Build and usage guide
- **[genesis-protocol/README.org](genesis-protocol/README.org)** - Genesis specification
- **[revelation-observer/README.md](revelation-observer/README.md)** - Structural diffusion guide
- **[integration/README.md](integration/README.md)** - Integration examples

## Status

**Production Ready**:
- ✅ CANB assembler/disassembler (deterministic, tested)
- ✅ Genesis fingerprinting (16-atom database generated)
- ✅ Revelation Observer (8-pass structural diffusion)
- ✅ SVG projection (2D Fano plane visualization)
- ✅ GLB projection (3D Merkaba export)
- ✅ Customizable P₀/P₁ symbols
- ✅ Fano-gate CANB validator (octonionic closure checking)
- ✅ Comprehensive documentation (architecture, guides, reference)

**In Progress**:
- 🔨 Peer-to-peer sync demo
- 🔨 GLB with Genesis provenance metadata

## Philosophy

> **From counting to relating.**
> **From numbers to geometry.**
> **From configuration to observation.**
> **From execution to traversal.**

This is not an incremental improvement—it is a **change in what we consider computation to be**.

## License

See LICENSE file.

## Contact

Brian Thorne
