# EVR Module Laws (Track B)

## Purpose

Define canonical event doctrine for observational evidence across the existing stack.

Events are canonical observational evidence of system actions and outcomes; they do not create or modify semantic truth.

## Non-goals (for this module boundary)

- No protocol semantic changes.
- No identity derivation changes.
- No capability outcome changes.
- No logging backend or metrics-stack design.
- No dashboard/product workflow design.

## Boundary doctrine

- `EventKind` determines evidence shape.
- Events are projection/evidence artifacts, not authority artifacts.
- Events never define SID, descriptor truth, capability truth, or canonical state.

## Core laws

### 1. Observational-Only Law

Events record outcomes of canonical operations. They never author or mutate canonical outputs.

### 2. EventKind Evidence Law

Every `EventKind` has one deterministic evidence schema. Equivalent operations emit equivalent evidence shapes.

### 3. Family/Kind Legality Law

`family` and `kind` must be legal pairs from the EVR inventory. Illegal pairings are malformed.

### 4. Source/Layer Legality Law

Each `EventKind` has an allowed source layer set. Events claiming impossible source/layer combinations are invalid.

### 5. Causal Linkage Law

Events that represent outcomes must carry deterministic causal linkage fields to the triggering operation (for example request digest, SID, call ref, or route id as applicable).

### 6. Timestamp/Sequence Law

Timestamp fields are observational metadata; deterministic ordering for verification is established by canonical event encoding plus append order/sequence fields where present.

### 7. Deterministic Encoding Law

Event artifacts must have stable canonical byte representation for hashing, replay, and fixture comparison.

### 8. Reject Taxonomy Law

Malformed events are typed deterministically (invalid shape, illegal kind, illegal source, missing causal link, invalid timestamp/sequence, etc.).

## Event inventory

## Implemented families (required in this module)

- `resolution.*`
- `descriptor.*`
- `capability.*`
- `adapter.*`
- `route.*`
- `hub.*`

## Reserved families (declared, not required in this module)

- `federation.*`
- `device.*`

## Canonical event envelope (module-level)

Minimum structural contract:

- `family` (string)
- `kind` (string)
- `source_layer` (`substrate|protocol|core|network|hub`)
- `ts` (RFC3339 timestamp string)
- `seq` (monotonic integer in stream scope where applicable)
- `evidence` (object; schema fixed by `EventKind`)

## Non-authority rule

- Events do not define identity.
- Events do not alter capability verdicts.
- Events do not redefine canonical protocol or substrate state.
