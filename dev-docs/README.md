# Tetragrammatron

**Deterministic Semantic Infrastructure**

> *Canonical meaning produces identity, identity produces addressability, and computation returns to meaning.*

---

## Stack

```
┌─────────────────────────────────────────────────────────┐
│  Hub          role-based projection shell · event registry │
├─────────────────────────────────────────────────────────┤
│  Network      semantic addressing · routing · federation  │
├─────────────────────────────────────────────────────────┤
│  Core         node runtime · four canonical endpoints     │
├─────────────────────────────────────────────────────────┤
│  Protocol     semantic calculus · canonical form · SID    │
├─────────────────────────────────────────────────────────┤
│  Substrate    content-addressed storage · log · replay    │
└─────────────────────────────────────────────────────────┘
```

Each layer depends only on the layers below it. The substrate has no dependencies.

---

## Doctrine

```
Identity is canonical.
Authority is delegated.
Credentials are adapters.
Descriptors are discoverable.
Presentation is projection.
Procedures are addressed by meaning.
Operators observe canonical truth; they do not author it.
EventKind determines evidence shape.
The substrate stores; it does not mean.
```

---

## Repositories

| Repository | Layer | Specifications |
|---|---|---|
| [tetragrammatron-substrate](#) | Substrate | NRR-1.0 |
| [tetragrammatron-protocol](#) | Protocol | DBC-1.2 · DBC-IDL-1.2 |
| [tetragrammatron-core](#) | Core | DBC-NODE-1.0 |
| [tetragrammatron-network](#) | Network | HD-RPC-1.0 |
| [tetragrammatron-hub](#) | Hub | HD-RPC-GUI-1.0 · HD-RPC-EVR-1.0 |
| [tetragrammatron-adapters](#) | Adapters | adapter:ipv6 · adapter:evm · adapter:did · adapter:git |
| [tetragrammatron-fixtures](#) | Fixtures | golden artifacts · replay traces |
| [tetragrammatron-conformance](#) | Conformance | law-to-test evidence mapping |

---

## Start here

**System architect:** [ARCHITECTURE.md](./ARCHITECTURE.md) → `tetragrammatron-protocol` → `tetragrammatron-network`

**Runtime implementer:** `tetragrammatron-substrate` → `tetragrammatron-protocol` → `tetragrammatron-core`

**Service provider:** `tetragrammatron-protocol` → `tetragrammatron-network` → `tetragrammatron-hub`

**Device / IoT integrator:** `tetragrammatron-substrate` → `tetragrammatron-adapters` → `tetragrammatron-core`

**Operator / GUI author:** `tetragrammatron-hub` → `tetragrammatron-core`

**Conformance test author:** `tetragrammatron-conformance` → `tetragrammatron-fixtures`

---

## Demo

A working end-to-end demonstration of the full stack loop:

```
document → normalize → SID → NRR → resolve → project
```

```bash
git clone https://github.com/tetragrammatron/tetragrammatron-demo
cd tetragrammatron-demo
node src/node.js
# open http://localhost:3000
```

No build step. No external dependencies. Node 18+.

---

*Tetragrammatron · NRR-1.0 · DBC-1.2 · DBC-IDL-1.2 · DBC-NODE-1.0 · HD-RPC-1.0 · HD-RPC-GUI-1.0 · HD-RPC-EVR-1.0*
