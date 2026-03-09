# Federation Module Fixture Matrix (Track C)

This matrix defines required federation fixture coverage before runtime integration hardening.

## Golden fixtures

### F-G01 Announce/Discover success

- valid provider announcement(s)
- deterministic discover result includes expected candidates

### F-G02 Multi-provider reachability success

- two or more reachable providers for same SID/scope
- candidate set is deterministic

### F-G03 Deterministic arbitration success

- identical candidate set and inputs
- same selected route target

### F-G04 Replay convergence success

- independent nodes over same canonical inputs
- convergence judgment is `converged`

### F-G05 Routed call equivalence success

- federated call result equivalent to direct Core call for selected route

## Negative fixtures

### F-N01 Conflicting provider announcement

- inconsistent descriptor claims for same provider identity
- typed failure: `announcement_conflict`

### F-N02 Unreachable route

- discovered provider unavailable at call time
- typed failure: `route_unreachable`

### F-N03 Replay divergence

- replay outcomes mismatch for scoped workload
- typed failure: `replay_divergence`

### F-N04 Stale/invalid federation descriptor

- malformed or integrity-failed descriptor
- typed failure: `invalid_federation_descriptor`

### F-N05 Arbitration ambiguity

- tie not resolvable under configured deterministic policy
- typed failure: `route_ambiguity`

### F-N06 Schema/federation-scope mismatch

- scope or schema incompatibility in candidate/provider
- typed failure: `scope_schema_mismatch`

## Determinism fixtures

### F-D01 Same announce set -> same route set

- repeated discovery over same announce inputs
- deterministic candidate result

### F-D02 Same topology -> same arbitration outcome

- repeated arbitration over same topology
- deterministic selected route or same typed failure

### F-D03 Same canonical inputs -> same convergence judgment

- repeated convergence checks over equivalent canonical artifacts
- deterministic `converged`/typed divergence result

### F-D04 Same divergence case -> same typed failure/evidence schema

- repeated divergent fixture input
- deterministic failure kind and evidence shape

## Integration fixtures

### F-I01 Discover -> arbitrate -> call pipeline

- discovery emits candidates
- arbitration selects target
- routed call succeeds or typed fails deterministically

### F-I02 Federated call + replay convergence witness

- federated call evidence ties to replay-convergence judgment

### F-I03 Capability context passthrough under federation

- capability context preserved end-to-end without reinterpretation

### F-I04 Hub federation inspection projection

- Hub inspection equals canonical underlying federation state

## Deployment profile fixture scope

Implemented profiles in this module:

- `local-multi-node` (deterministic witness profile)

Deferred profiles:

- wide-area peer mesh
- mobile/embedded constrained profile
