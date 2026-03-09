# Track B Charter — EVR / Observability Doctrine

## Objective

Define and implement canonical event families and evidence shapes as observational outputs only.

## In

- Event family taxonomy
- EventKind -> evidence shape mapping
- Event identity/immutability rules
- Core/Network/Hub emission and subscription contracts

## Out

- Capability grant semantics
- Federation route arbitration policies
- Product UI role segmentation

## Acceptance gates

- Deterministic event emission for canonical operations
- No-backflow: events do not author canonical truth
- Event conformance tests for shape stability and completeness

## Milestones

- B1: EVR schema + event fixtures
- B2: emitter wiring + hub event projections
- B3: conformance and checklist integration
