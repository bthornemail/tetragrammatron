# **CONTRIBUTING.md**

## Contributing to Tetragrammatron-OS

This project is not built by adding features.
It is built by **preserving invariants** while discovering better projections.

If you contribute, you are participating in a **semantic system**, not just a codebase.

---

## 1. What Contributions Are Welcome

You may contribute:

* Clarifications of existing ideas
* New projections (POSIX, WASM, SVG, hardware, etc.)
* Simplifications that preserve meaning
* Implementations that faithfully project relations
* Counterexamples that reveal hidden assumptions
* Documentation that improves observability

You do **not** need permission to explore.

---

## 2. What Contributions Are Not Accepted

We do not accept changes that:

* Introduce numbers as canonical truth
* Collapse relations into metrics
* Assume clocks define correctness
* Encode meaning into hardware properties
* Treat POSIX, Nix, or any OS as foundational
* Add abstraction without observational grounding

If a contribution requires a number:

* it must be derived
* it must be ephemeral
* it must not be stored as authority

---

## 3. The Rule of Invariants

Before submitting a change, ask:

* What invariant does this preserve?
* What boundary does this respect?
* What projection does this introduce?
* What assumptions does this remove?

If you cannot answer these, the change is not ready.

---

## 4. RFC-Driven Development

Substantial changes must be accompanied by an RFC or RFC update.

An RFC is:

* a recorded understanding
* a frozen conversation
* a statement of alignment

RFCs explain *why*, not just *what*.

---

## 5. Numbers, Indices, and Metrics

Numbers are allowed only as:

* derived views
* queries
* temporary computation artifacts

Numbers must never:

* define identity
* define ordering semantics
* define meaning

If two entities differ only by number, they are the same entity.

---

## 6. Implementation Guidelines

When implementing:

* Prefer relations over structures
* Prefer projection over translation
* Prefer observation over assumption
* Prefer idempotence over optimization
* Prefer reversibility over speed

If something cannot be reversed, it must be justified.

---

## 7. Disagreement and Forking

Disagreement is expected.

* Forking is allowed
* Divergence is acceptable
* Silent semantic drift is not

If you disagree:

* explain the invariant you believe is violated
* show the alternative projection
* preserve reversibility

---

## 8. Contribution Summary

> You are not adding features.
> You are refining a worldview made executable.
> Preserve meaning. Everything else is negotiable.
