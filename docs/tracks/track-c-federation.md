# Track C Charter — Federation Breadth

## Objective

Expand HD-RPC from minimal local witness routing to deterministic multi-provider federation behavior.

## In

- Peer discovery
- Route arbitration policy
- Multi-provider convergence proofs
- Deployment profile docs and tests

## Out

- Capability doctrine authoring (consumes Track A outputs only after freeze)
- UI role model changes
- EVR family redefinition

## Acceptance gates

- Multi-node convergence tests pass under independent initialization
- Deterministic route selection/fallback behavior
- Clear typed failures for discovery/routing mismatches

## Milestones

- C1: peer model + discovery fixtures
- C2: arbitration + deterministic routing tests
- C3: deployment profile hardening + conformance updates
