# ABI Fixture Matrix (Track F)

## Golden

- ABI-G-01 ResolveCall -> ResolveResult (Normalized)
- ABI-G-02 ResolveCall -> RejectEnvelope (typed reject)
- ABI-G-03 IdentityDescriptor canonical shape
- ABI-G-04 DescriptorVerificationResult valid
- ABI-G-05 CapabilityVerificationResult valid
- ABI-G-06 CapabilityVerificationResult expired
- ABI-G-07 RevocationRecord + RevocationVerificationResult revoked
- ABI-G-08 EVREvent legality (resolution kind)
- ABI-G-09 EVREvent legality (capability kind)
- ABI-G-10 FederationAnnouncement
- ABI-G-11 ConvergenceWitness
- ABI-G-12 DivergenceWitness

## Negative

- ABI-N-01 missing required field
- ABI-N-02 stage/value_kind mismatch
- ABI-N-03 reject_kind outside taxonomy
- ABI-N-04 EVR missing required causal evidence
- ABI-N-05 unknown event kind
- ABI-N-06 unsorted runtime_sids in witness
- ABI-N-07 status outside closed taxonomy
- ABI-N-08 invalid digest shape
- ABI-N-09 invalid sid shape
- ABI-N-10 unsorted NormalForm edges

## Determinism

- ABI-D-01 same NormalForm => same SIDOutput
- ABI-D-02 same ResolveCall => byte-identical ResolveResult encoding
- ABI-D-03 same capability state => stable CapabilityVerificationResult
- ABI-D-04 same EVR event => stable EVR encoding
- ABI-D-05 same descriptor input => same descriptor digest
