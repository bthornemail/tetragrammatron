# RFC-0016 — Hardware-as-Relations Projection Layer (Normative)

## 1. Goal

Define a platform-agnostic way to represent hardware as a **relation algebra**, producing:

* **HARDWARE GRAPH** (canonical): atoms + labeled relations
* **OBSERVATIONS** (non-canonical): measured fields (freq, RAM, temps, etc.)
* A projection into the VM’s semantic boundary (your “sphere”) via **Fano-safe constraints**, without trusting numeric IDs.

## 2. Core Principles

1. **No numeric identity is canonical.**
   Serial numbers, MACs, bus addresses, device node minors are *observations*.
2. **Relations are primary.**
   Everything is an atom connected by typed edges.
3. **Measurements are optional and discardable.**
   They help scheduling, QoS, UI, tuning—but never define identity.
4. **Projection is lossful and idempotent.**
   `proj(proj(x)) = proj(x)`.

## 3. Canonical Data Model (Relation Algebra)

### 3.1 Atoms

`Atom` is an opaque identifier only meaningful inside a single graph instance.

Canonical atom kinds (minimum set):

* `HOST`
* `CPU`
* `CORE`
* `ISA`
* `MEMORY`
* `BUS`
* `DEVICE`
* `NETIF`
* `RADIO`
* `STORAGE`
* `SENSOR`
* `CLOCK`
* `POWER`

### 3.2 Relations (Edges)

Each edge is a triple:

`(src_atom, REL, dst_atom)`

Minimum relation vocabulary:

* `HAS` (containment/part-of)
* `BOUND_TO` (device ↔ driver/module)
* `EXPOSES` (device ↔ capability)
* `SUPPORTS` (CPU ↔ ISA extension)
* `ATTACHED_TO` (device ↔ bus)
* `ROUTES_TO` (netif ↔ gateway/peer)
* `CLOCKED_BY` (component ↔ clock source)
* `POWERED_BY` (component ↔ power domain)
* `OBSERVED_AS` (atom ↔ observation record)

### 3.3 Observations (Non-canonical)

Observation record is a bag of facts that may include numbers:

`Obs = { key: Symbol, value: Scalar, unit?: Symbol, source: Symbol, t?: TimeTag }`

Rules:

* Observations **MUST NOT** be used as stable identity.
* Observations **MAY** be hashed for local caching.
* Observations **MUST** carry a `source` tag (`/proc`, `sysfs`, `esptool`, `termux-api`, etc.)

## 4. Capability Lattice (Pure)

Capabilities are *names*, not measurements:

Examples:

* `cap:isa:rv32i`, `cap:isa:xtensa`, `cap:isa:armv7m`
* `cap:net:wifi`, `cap:net:ble`, `cap:net:ethernet`
* `cap:io:i2c`, `cap:io:spi`, `cap:io:uart`
* `cap:storage:flash`, `cap:storage:sd`
* `cap:clock:rtc`, `cap:clock:xtal`, `cap:clock:pll`

Encode as edges:

`(DEVICE, EXPOSES, cap:net:wifi)`

## 5. Hardware→VM Projection (Fano-safe)

Define a projection operator:

`Π_hw : HardwareGraph → VMSemantics`

But *VMSemantics* must be **pure relational residues**, not numeric IDs.

### 5.1 Residue Construction

Instead of `%8`, use **octonionic basis naming** (character tokens):

Basis symbols:
`{E0,E1,E2,E3,E4,E5,E6,E7}` (as strings)

We derive a residue **set** (not number):

`Residue(h) = {Ei | predicate_i(h) holds}`

Example predicates (illustrative):

* `E0` if host has any CPU
* `E1` if any `cap:net:*`
* `E2` if any `cap:storage:*`
* `E3` if any `cap:io:*`
* `E4` if any `cap:sensor:*`
* `E5` if any `cap:clock:*`
* `E6` if any `cap:power:*`
* `E7` if multi-node federation present (`ROUTES_TO` edges exist)

This gives an **8-symbol mask** without numbers.

### 5.2 Admissibility (Sphere Membership)

A hardware profile is admissible if its residue passes your Fano gate:

`Admissible(h) := FANO_OK(Residue(h))`

Where `FANO_OK` is the same predicate family you use for merge gates / triads.

## 6. Output Artifacts

From one probe run, emit:

1. `hardware.canvasl` (canonical graph edges)
2. `hardware.obs.yaml` (observations: human-readable)
3. `hardware.obs.jsonl` (stream: replay/debug)
4. `hardware.residue.canvasl` (the 8-symbol residue + proof hooks)

