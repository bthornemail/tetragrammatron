# Track Workflow Rules

## Branch naming

- `track-a/<topic>` capability and authority model
- `track-b/<topic>` EVR and observability doctrine
- `track-c/<topic>` federation breadth
- `track-d/<topic>` productization
- `track-e/<topic>` revocation

## Required labels

- `track:a-capability`
- `track:b-evr`
- `track:c-federation`
- `track:d-productization`
- `track:e-revocation`
- Optional: `contract-impact`, `snapshot-update`

## PR discipline

- Each PR targets one track only.
- PRs touching frozen contracts must include:
  - contract delta
  - migration note
  - conformance impact
- Snapshot changes require explicit `snapshot-update` label and reviewer signoff.

## CI lanes

- `ci-track-a`
- `ci-track-b`
- `ci-track-c`
- `ci-track-d`
- `ci-track-e`
- `ci-contract-integration`

A PR is merge-eligible only when its track lane and `ci-contract-integration` are green.

## Release discipline

- Do not cut releases for partial features.
- Cut releases only for law-complete, fixture-complete, test-complete modules.
- `v1.0.0-semantic-baseline` is the frozen semantic baseline.
- Use `docs/release-policy.md` and `docs/modules/module-release-checklist.md` before tagging.
