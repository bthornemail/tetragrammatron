# Hub Roles Module Release Checklist

## Law completeness

- [x] Role doctrine complete for Provider/Broker/Consumer/User/Agent.
- [x] Workspace doctrine complete for A/B/C/D/E/F.
- [x] Shell laws explicit: no-mutation, endpoint-fidelity, no-backflow, role-scope, projection-read-only, broker-verbatim-forward.

## Fixture completeness

- [x] Golden role/workspace fixtures complete.
- [x] Negative scope/fidelity/read-only/rewrite fixtures complete.
- [x] Determinism fixtures complete.
- [x] Integration fixtures complete.

## Implementation/test completeness

- [x] Role-scoped command authorization enforced in shell.
- [x] Workspace views are projection-only over existing contracts.
- [x] Broker forwarding witness implemented and tested.
- [x] User portal read-only constraints enforced and tested.
- [x] EVR family-to-workspace mapping implemented and tested.

## Release artifacts

- [x] CHANGELOG includes Track H section.
- [x] README includes role/workspace run surfaces.
- [x] Conformance checklist includes role/workspace shell laws.
- [x] Module manifest updated for hub-roles module.
