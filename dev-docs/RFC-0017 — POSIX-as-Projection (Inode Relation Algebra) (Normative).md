# RFC-0017 — POSIX-as-Projection (Inode Relation Algebra) (Normative)

## 1. Goal

Define a POSIX compatibility layer as a **derived view** of the same relation graph.

POSIX entities are projections:

* Paths = repeated `CONTAINED_IN` traversal
* Directories = `MAP name→inode`
* Files = inode atoms with `BACKING data`
* FDs = ephemeral session-local handles
* Mounts = overlays / rebinds (relations), not “special objects”
* Sockets = `CONNECTED_TO` relations

## 2. Canonical Inode Relation Algebra (IRA)

### 2.1 Atoms

* `INODE` (opaque)
* `NAME` (string atom)
* `DIR` (inode subtype)
* `FILE` (inode subtype)
* `DEVICE_NODE` (inode subtype)
* `SOCKET_NODE` (inode subtype)
* `DATA` (content atom, could be CAS)
* `PROC` (process atom)
* `FD` (ephemeral handle atom)
* `MOUNT` (overlay atom)
* `NS` (namespace atom)

### 2.2 Relations

Minimum relations:

* `CONTAINS(DIR, INODE)`
* `BINDS_NAME(DIR, NAME, INODE)`  (directory entry)
* `BACKED_BY(FILE, DATA)`
* `HAS_FD(PROC, FD)`
* `FD_REFERS_TO(FD, INODE)`
* `LINKS_TO(INODE, INODE)`  (symlink as relation)
* `OVERLAYS(MOUNT, INODE_upper, INODE_lower)`
* `VISIBLE_AS(NS, INODE, PATHVIEW)` (projection output)
* `CONNECTED_TO(SOCKET_NODE, PEER)`

**Important:** `inode_number` is an observation, never canonical.

## 3. Hard Links as Pure Relation

POSIX hard link is *not* “copy data”.

It is simply:

Two directory entries bind to the **same inode**:

`BINDS_NAME(dirA, "x", i)`
`BINDS_NAME(dirB, "y", i)`

No extra structure required.

## 4. Mounts as Overlay Relations

A mount is an overlay/rebinding:

`OVERLAYS(m, upper_inode, lower_inode)`

A namespace projection resolves “what path shows” by walking overlays.

So “mount points” are not objects; they are **resolution rules**.

## 5. Paths as Derived Traversals

A POSIX path string is a *query result*, not stored truth:

`Resolve("/a/b/c") := walk(BINDS_NAME, starting at NS root)`

Paths do not define identity.

Identity is the inode atom + relations.

## 6. File Descriptors are Ephemeral

FDs must be treated as:

* session-local
* process-local
* discardable

Model:

`HAS_FD(proc, fd123)`
`FD_REFERS_TO(fd123, inodeX)`

The integer “3” is an observation emitted by the projection.

## 7. POSIX Projection Operator

Define:

`Π_posix : IRA × Namespace × Policy → POSIXView`

Outputs:

* directory listings (name→fd/inode projections)
* read/write streams (DATA mapping)
* permission bits (policy-produced observations)
* stat-like fields (observations only)

### 7.1 The “stat” boundary

`stat` fields are “event-horizon” outputs: they’re acceptable because they’re explicitly a projection.

Rules:

* do not store stat numbers as canonical facts
* only cache them with a timestamp/source tag
* treat change as normal (it’s the view changing)

## 8. VM Integration

The VM does not “implement POSIX”.

The VM implements:

* atoms + relations
* a query engine
* a projection `Π_posix` for compatibility

So your system can:

* run on Linux / Android / ESP32 (limited)
* project into POSIX where it exists
* emulate POSIX where you want it (optional)
