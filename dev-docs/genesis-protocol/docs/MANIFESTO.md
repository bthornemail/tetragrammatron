# **MANIFESTO.md**

## The Tetragrammatron-OS Manifesto

This manifesto defines **what we will build**, **what we refuse to build**, and **why**.

---

## 1. Relations Over Numbers

We reject numeric primacy.

* Numbers are derived views
* Counts are queries
* Sizes are interpretations
* Indices are conveniences, not truth

All canonical state is relational.

---

## 2. Semantics Are Invariant

Semantics must not depend on:

* hardware speed
* clock frequency
* memory layout
* address width
* file descriptor numbering

Semantics live in a **sphere of invariance**.
Everything else is projection.

---

## 3. Hardware Is Observed, Not Trusted

Hardware reports are:

* measurements
* claims
* observations

They are **bounded by physics**, not by vendor specification.

The VM projects hardware into semantics.
Hardware never defines semantics.

---

## 4. POSIX Is a Projection, Not a Foundation

We support POSIX.
We do not worship POSIX.

Paths, files, mounts, and descriptors are:

* derived
* ephemeral
* session-local

The canonical model is relational.

---

## 5. Determinism Without Clocks

Time is ordering.
Not ticks.
Not cycles.
Not nanoseconds.

Execution is:

* causally ordered
* replayable
* invariant under speed changes

---

## 6. Visualization Is Optional

SVG, OBJ, GLB, Canvas, AR/VR are **views**, not state.

They must:

* faithfully project relations
* never become authoritative
* be discardable

---

## 7. Growth and Reduction Are Symmetric

Expansion and compression are duals.

* Nothing is lost
* Nothing is privileged
* Reduction is not destruction
* Growth is not inflation

---

## 8. We Build for the Unknown Future

We do not predict future hardware.
We **bound it**.

Anything physically possible must:

* either project cleanly
* or be rejected safely

There are no unknown unknowns—only unobserved relations.

---

## 9. Manifesto Summary

> Computation should resemble reality, not accounting.
> Meaning should survive execution.
> Systems should align, not dominate.

This is Tetragrammatron-OS.

---

---
