# Canvas Style Guide (Advisory)

## Readability goals

- Make flow intent obvious in under 10 seconds.
- Keep labels short and concrete.
- Prioritize clarity over visual density.

## Layout conventions

- Place primary flow left-to-right.
- Place dependency stacks top-to-bottom.
- Keep related nodes spatially grouped.
- Keep identity anchors visually central when helpful.

## Edge and connector guidance

- Minimize edge crossings.
- Prefer straight or gently curved edges.
- Avoid long crossing back-edges unless explicitly labeled.

## Label clarity

- Use explicit operation labels (`resolve`, `deriveSID`, `call`).
- Avoid ambiguous labels like `process` or `do`.
- Mark compatibility-only paths explicitly (for example `ipv4 compatibility`).

## Grouping rules

- Group by layer when describing architecture.
- Group by scenario when describing a workflow/demo.
- Avoid mixed grouping in the same diagram unless necessary.

## Geometry and semantics rule

- Do not encode protocol meaning in geometry alone.
- Diagrams are explanatory projections, not authoritative semantics.
