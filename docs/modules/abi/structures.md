# ABI Structures (Track F)

## Protocol

- `ResolveCall`
- `ResolveResult`
- `RejectEnvelope`
- `NormalForm`

## Identity

- `SIDOutput`
- `IdentityDescriptor`
- `DescriptorVerificationResult`

## Authority

- `CapabilityVerificationResult`
- `RevocationRecord`
- `RevocationVerificationResult`

## EVR

- `EVREvent`

## Federation

- `FederationAnnouncement`
- `RouteSet`
- `ArbitrationResult`
- `ConvergenceWitness`
- `DivergenceWitness`

## Closed taxonomies

- Descriptor status: `valid|digest_mismatch|epoch_invalid|schema_mismatch|not_found`
- Capability status: `valid|expired|revoked|invalid_signature|broken_chain|schema_mismatch`
- Revocation status: `not_revoked|revoked|record_invalid|record_not_found`
- Federation arbitration: `converged|diverged|inconclusive`
