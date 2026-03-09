# Hub Roles Module Laws (Track H)

## Track boundary

Track H reveals existing semantics through role-shaped projection; it does not create new semantic authority, policy, or protocol law.

## Purpose

Complete role-segmented Hub projections over frozen Core/Network/EVR contracts.

## Non-goals

- No canonical semantic mutation from Hub.
- No new protocol endpoint definitions.
- No authn/authz redesign or policy-engine expansion.
- No routing/discovery heuristics owned by Hub.

## Role doctrine

- Provider: publish and inspect meaning projections.
- Broker: verify/route/forward without payload reinterpretation.
- Consumer: request stage execution and inspect deterministic results.
- User: projection-only portal, read-only.
- Agent: full-stack bootstrap/inspection console over existing APIs.

## Role-to-endpoint matrix

| Role | Primary endpoint | Secondary endpoints | Read-only surfaces |
|---|---|---|---|
| Provider | `POST /resolve` | `GET /sid/{digest}` | capability inspection |
| Broker | `POST /verify-capability` | `GET /sid/{digest}`, `GET /adapter/ipv6/{sid}` | federation view |
| Consumer | `POST /resolve` | `GET /sid/{digest}` (read-only) | none |
| User | none | `GET /sid/{digest}` (projection-only) | all |
| Agent | `POST /resolve` (bootstrap) | all four node endpoints | all |

## Workspace doctrine

- A Identity: descriptor and adapter projections.
- B Resolution: resolve request/response projection.
- C Capabilities: capability and revocation verification projection.
- D Routing: route/adapter inspection and broker forwarding witness.
- E Federation: provider/routeset/arbitration/convergence inspection.
- F Projections: read-only event/store/descriptor/routing timeline.

## Core shell laws

1. No Direct Mutation Law
- Hub writes canonical state only through `POST /resolve` projection path.

2. Endpoint Fidelity Law
- Every panel maps to one existing endpoint/EVR surface.

3. No Backflow Law
- Projection UI state cannot redefine protocol/core/network outputs.

4. Role Scope Law
- Role determines visible workspace set and command authorization.

5. Projection Read-Only Law
- User portal is read-only; no mutating actions are authorized.

6. Broker Verbatim Forward Law
- Broker forwarding must preserve request payload bytes/structure semantics.

7. Event-Driven Visibility Law
- Workspace feeds are EVR-backed by family/kind evidence maps.

8. Digest Verification Law
- Descriptor/artifact digest values are displayed and compared from canonical artifacts.

## Determinism claim (module)

Same canonical node/descriptor/event inputs produce the same role-scoped shell state, with no semantic invention by Hub.
