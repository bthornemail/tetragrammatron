# Federation Module Laws (Track C)

## Purpose

Define deterministic federation coordination for discovery, reachability, arbitration, and convergence over already-canonical semantics.

Federation coordinates reachability and convergence of already-canonical semantics; it does not create canonical truth.

## Non-goals (for this module boundary)

- No protocol semantic changes.
- No SID derivation changes.
- No capability verdict changes.
- No EVR taxonomy changes.
- No peer-mesh optimization heuristics (retry/backoff tuning, latency scheduling, etc.).
- No device bootstrap doctrine.
- No role-based product workflow design.

## Boundary doctrine

- Discovery/reachability concerns operational path selection, not semantic validity.
- Arbitration chooses among reachable providers; it does not reinterpret Core or Protocol outcomes.
- Convergence is judged by replay/canonical-artifact agreement, not by coordination side effects.
- Hub inspection is projection-only.

## Core laws

### 1. Discovery/Announcement Law

Providers may announce federation descriptors and route capabilities through deterministic descriptor envelopes. Discovery produces candidate reachability sets only.

### 2. Reachability-Is-Not-Validity Law

A reachable provider is not semantically authoritative by reachability alone. Semantic validity remains defined by Protocol/Core outputs.

### 3. Arbitration Determinism Law

Given identical candidate provider sets and identical arbitration inputs, route selection outcome must be deterministic and reproducible.

### 4. Arbitration Non-Invention Law

Arbitration may choose route targets or produce typed route failures, but must not change request semantics, stage semantics, SID semantics, or capability semantics.

### 5. Replay Convergence Law

Independent nodes converge when replay and canonical artifact checks agree for the scoped workload. Convergence claims require explicit evidence inputs and outputs.

### 6. Divergence Typing Law

Divergence and mismatch outcomes are typed deterministically (for example: descriptor mismatch, schema mismatch, scope mismatch, replay mismatch, route ambiguity).

### 7. Federation Descriptor Integrity Law

Federation descriptors used for discovery/arbitration must be structurally valid, integrity-checkable, and scope-bounded.

### 8. Deployment Profile Separation Law

Implemented deployment profiles must be explicit and test-backed. Deferred profiles must be declared, not implied.

### 9. Non-Authority Law

Federation does not define canonical truth, identity, or authority. It coordinates transport-level reachability and convergence observation only.

## Layer interaction boundaries

### Protocol boundary

Federation never alters canonical NormalForm, SID derivation, or protocol reject semantics.

### Capability boundary

Federation may carry capability context but may not reinterpret capability verification outcomes.

### EVR boundary

Federation may emit/consume `route.*` and future `federation.*` events as observational evidence only; it does not redefine EVR laws.

### Hub boundary

Hub surfaces federation state as read-only projection over Network/Core/EVR artifacts.

## Implemented vs deferred family scope

Implemented in this module:

- `route.*` federation extension behaviors
- `federation.*` minimal announce/discover/arbitrate/converge witness kinds

Deferred in this module:

- `device.*` family completion
- large-scale mesh optimization policies
