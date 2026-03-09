# Replay Witness Pack

Replay Witness Packs are self-contained conformance artifacts for deterministic replay judgments.

## Required structure

```text
replay-pack/
  manifest.json
  profile.json
  policy.json
  invocation.json
  semantic-result.json
  evidence/
  evr-events.json
  expected/
    final-verdict.json
    canonical-digests.json
    replay-trace.json
```

## Core rule

A replay pack must preserve invocation, semantic result, evidence trail, event trail, and explicit judgment policy.

## profile.json

Declares target verifier class:

- `semantic-reader`
- `semantic-verifier`
- `runtime-host`
- `full-node`

## policy.json

Declares judgment behavior:

- `verdict_policy_version`
- `must_match`
- `may_vary`
- `ordering`
- `failure_class`
- `verdict_strength` (`strict`, `equivalence`, `advisory`)

## Closed verdict classes

- `pass`
- `semantic_mismatch`
- `execution_mismatch`
- `event_mismatch`
- `policy_inapplicable`

## Canonical packs (v1)

- `replay-packs/resolve-success`
- `replay-packs/resolve-reject`
- `replay-packs/capability-revoked`
- `replay-packs/routed-call`
- `replay-packs/federation-divergence`

## Verification

- Verify one pack: `npm run replay:verify -- replay-packs/resolve-success`
- Verify all packs: `npm run replay:verify:all`
