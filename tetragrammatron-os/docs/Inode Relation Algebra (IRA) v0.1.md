### Inode Relation Algebra (IRA) v0.1

**INODE = Atom + Relations** (numbers are *views*, never facts)

---

## 1) Core model

### 1.1 Atoms

* `Inode` : Atom (opaque identity, content-addressed or derived)
* `Name`  : Atom (UTF-8 token)
* `ByteBlob` : Atom (content hash / backing store pointer)
* `Driver` / `Device` / `Peer` : Atom

No numeric IDs are canonical. Any integer you see (inode number, fd) is a **session token**.

### 1.2 Relations (all boolean, no arithmetic)

Let `Rel ⊆ Atom × Atom`. The inode universe is a typed subgraph:

**Structural**

* `HAS_BACKING(inode, blob)`
* `MAPS_NAME(dir_inode, name_inode)` *(directory entry node)*
* `ENTRY_NAME(entry, name)`
* `ENTRY_TARGET(entry, inode)`
* `CONTAINED_IN(inode_or_entry, dir_inode)` *(parent edge)*

**Overlay / Mount**

* `OVERLAY(parent_dir, child_root_dir)` *(mount intent)*
* `RESOLVES(parent_dir, effective_view_dir)` *(computed view)*
* `SHADOWS(entryA, entryB)` *(name collision precedence)*

**Special file classes**

* `BOUND_TO(dev_inode, driver)`
* `CONNECTED_TO(sock_inode, peer)`
* `LINKS_TO(symlink_inode, path_atom)` *(symbolic text path)*

**Permissions / metadata (still relational)**

* `OWNS(inode, principal)`
* `HAS_MODE(inode, mode_atom)` *(e.g. "u+rwx,g+rx,o+rx" as token)*
* `HAS_TIME(inode, time_atom)` *(timestamp token; numeric is a derived lens)*

---

## 2) Directory and path semantics as pure traversal

### 2.1 Directory entries are explicit nodes (important)

A POSIX directory isn’t “a map” inside the inode. It’s a graph pattern:

A directory inode `D` **contains** *entry nodes* `E`, each of which binds a `Name` to a target `Inode`.

Formally:

* `CONTAINED_IN(E, D)`
* `ENTRY_NAME(E, N)`
* `ENTRY_TARGET(E, I)`

This is the whole “name→inode” map, without any numbers.

### 2.2 Paths are derived views

A path like `/a/b/c` is a **query**:

`PATH_RESOLVE(root, [a,b,c]) = I` where each step is:

1. Find entry `E` with `CONTAINED_IN(E, CurDir)` and `ENTRY_NAME(E, NameAtom)`
2. Set `CurDir := ENTRY_TARGET(E)`

So: **paths = repeated CONTAINED_IN traversal** + name matching.

> No path is stored as truth. Only edges are stored.

---

## 3) Hard links and mounts as relational collapse

### 3.1 Hard links (same inode, multiple entries)

POSIX “hard link” is just: two distinct entry nodes target the same inode.

* `ENTRY_TARGET(E1, I)`
* `ENTRY_TARGET(E2, I)`
* `ENTRY_NAME(E1, "x")`, `CONTAINED_IN(E1, D1)`
* `ENTRY_NAME(E2, "y")`, `CONTAINED_IN(E2, D2)`

There is **no extra object** called a hard link. It’s literally shared target identity.

**“link count”** becomes a derived lens:

* `NLINK(I) := COUNT({E | ENTRY_TARGET(E, I)})`
  …but per your rule: **count is a query result, never stored**.

### 3.2 Mounts (overlay relation, not a mutation)

A mount is not “moving directories”. It’s adding an **overlay rule**:

* `OVERLAY(parent_dir = /mnt, child_root = /dev/sda1-root)`

The *effective* directory view for traversal is computed by a projection function:

* `EFFECTIVE_VIEW(D) := overlay_fold(D)`

Where `overlay_fold`:

* unions entry sets from base dir and overlay root
* resolves name conflicts by precedence (overlay shadows base)
* yields `RESOLVES(D, D')` as a derived edge (optional cache)

So “mount table” is just a set of `OVERLAY` relations.

---

## 4) POSIX compatibility projection layer for your VM

### 4.1 What POSIX sees (a derived interface)

POSIX wants:

* `open(path) -> fd`
* `read(fd) -> bytes`
* `readdir(path) -> names`
* `stat(path) -> numbers`

Your VM provides:

* canonical relational state
* deterministic traversals
* byte-backed blobs

So define a projection:

`Π_posix : (RelGraph × Session) → PosixView`

Where `Session` carries ephemeral tokens:

* `FD : token → inode` map (session-local)
* `CWD : inode` current directory
* `ROOT : inode` root directory for namespace
* optional `MountViewCache`

### 4.2 Required projection invariants

* **Non-canonical**: `fd`, `inode_number` are session tokens only
* **Stable semantics**: path resolution depends only on relations + session root/cwd, not on numeric IDs
* **Replay-safe**: same relational graph + same session seed ⇒ same observable behavior

### 4.3 POSIX ops as graph queries/effects

**open(path, flags)**

1. `I := PATH_RESOLVE(Session.CWD or ROOT, tokenize(path))`
2. Allocate fresh `fd_token` (session-local)
3. Add session mapping: `FD_MAP(fd_token, I)`
4. Return `fd_token`

**read(fd, n)**

1. `I := FD_MAP(fd)`
2. `blob := HAS_BACKING(I)`
3. return `SLICE(blob, n, offset)` (offset is session state, not inode truth)

**readdir(path)**

1. `D := PATH_RESOLVE(...)`
2. `D_eff := EFFECTIVE_VIEW(D)` (honor overlays)
3. return `{ name | ∃E, CONTAINED_IN(E, D_eff) ∧ ENTRY_NAME(E, name) }`

**stat(path)**
Return a *view struct*:

* inode token: `ino = SESSION_NUMBER(I)` *(derived)*
* mode token, time tokens, size token derived from backing blob length

Numbers are emitted as **views**, never committed back into the canonical graph.

---

## 5) Minimal spec you can drop into an RFC section

### 5.1 IRA axioms (normative)

1. **Inode identity is atomic**: an inode is an Atom; its “number” is a view.
2. **Directory = entries**: directory membership is expressed only by `CONTAINED_IN(entry, dir)` plus `ENTRY_NAME/ENTRY_TARGET`.
3. **Hard link = shared target**: multiple entries may target the same inode.
4. **Mount = overlay relation**: mounts do not mutate directories; they add `OVERLAY` edges and change the projection.
5. **POSIX projection is sessioned**: file descriptors and inode numbers are session-local tokens.
6. **Counts are derived**: link count, size, inode numbers are computed on demand and not stored as facts.

### 5.2 Compatibility claim

Any POSIX filesystem behavior that depends only on:

* directory traversal
* name lookup
* shared inode identity (hard links)
* overlay/mount precedence
  is representable as IRA relations plus `Π_posix`.
