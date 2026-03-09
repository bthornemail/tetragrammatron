# Canvas Visualization Profile (Optional)

## Purpose

This profile defines optional conventions for creating Tetragrammatron diagrams in JSON Canvas.

## Non-authoritative rule

**JSON Canvas artifacts are projection-only diagram surfaces. They do not define protocol semantics, execution behavior, identity, authority, or conformance.**

## Flow orientation defaults

- Left -> Right: execution and routing flow.
- Top -> Bottom: dependency or layering flow.

## Suggested layer color mapping

- Red: routing / control
- Blue: protocol / computation
- Green: node execution / host logic
- Purple: storage / NRR
- Gray: observation / hub inspection

Color choices are advisory visualization hints only.

## Optional annotation prefixes

- `#Resolve`
- `#Normalize`
- `#Route`
- `#Store`
- `#Inspect`

Prefixes are optional labels for readability and are not machine-semantic.

## Example use

Use JSON Canvas for:

- architecture flow diagrams
- pipeline walkthrough diagrams
- federation topology sketches
- capability chain visualization (when capability module exists)

Do not use JSON Canvas artifacts as protocol inputs.
