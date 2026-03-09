import {
  ABI_VERSION,
  ADAPTER_STATUS,
  ARBITRATION_STATUS,
  CAPABILITY_STATUS,
  DESCRIPTOR_STATUS,
  REJECT_KIND,
  REVOCATION_STATUS,
  STAGE_KIND,
} from './schema.mjs';
import { abiReject } from './errors.mjs';
import { knownEventKind, requiredEvidenceFieldsForKind } from './kinds.mjs';

function hasKeys(obj, keys) {
  return keys.every((k) => Object.prototype.hasOwnProperty.call(obj, k));
}

function isSha256(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

function isSid(value) {
  return typeof value === 'string' && /^sid:dbc:[0-9a-f]{64}$/.test(value);
}

function checkAbiVersion(value) {
  return typeof value === 'string' && value === ABI_VERSION;
}

export function validateStructure(kind, value) {
  if (!value || typeof value !== 'object') {
    return abiReject('invalid_structure', { kind });
  }

  if (kind === 'ResolveCall') {
    if (!hasKeys(value, ['abi_version', 'canonical_input', 'schema_digest', 'target_stage', 'view_digest'])) {
      return abiReject('missing_field', { kind });
    }
    if (!checkAbiVersion(value.abi_version)) {
      return abiReject('invalid_abi_version', { kind, value: value.abi_version });
    }
    if (!STAGE_KIND.includes(value.target_stage)) {
      return abiReject('invalid_stage', { kind, value: value.target_stage });
    }
    if (!isSha256(value.schema_digest)) {
      return abiReject('invalid_schema_digest', { kind });
    }
    return { ok: true };
  }

  if (kind === 'ResolveResult') {
    if (!hasKeys(value, ['abi_version', 'canonical_value', 'schema_digest', 'stage', 'value_digest', 'value_kind'])) {
      return abiReject('missing_field', { kind });
    }
    if (!checkAbiVersion(value.abi_version)) {
      return abiReject('invalid_abi_version', { kind, value: value.abi_version });
    }
    if (!STAGE_KIND.includes(value.stage)) {
      return abiReject('invalid_stage', { kind, value: value.stage });
    }
    const stageKinds = {
      Closed: 'ClosedStructure',
      Normalized: 'NormalForm',
      Projected: 'ProjectionModel',
      Realized: 'RealizedStructure',
    };
    if (stageKinds[value.stage] !== value.value_kind) {
      return abiReject('invalid_value_kind', {
        expected: stageKinds[value.stage],
        got: value.value_kind,
        kind,
        stage: value.stage,
      });
    }
    if (!isSha256(value.value_digest) || !isSha256(value.schema_digest)) {
      return abiReject('invalid_digest', { kind });
    }
    if (value.stage === 'Normalized' && value.value_kind === 'NormalForm') {
      const edges = value.canonical_value?.edges ?? [];
      const sortedEdges = [...edges].sort((a, b) => JSON.stringify(a).localeCompare(JSON.stringify(b)));
      if (JSON.stringify(edges) !== JSON.stringify(sortedEdges)) {
        return abiReject('invalid_ordering', { field: 'edges', kind });
      }
    }
    return { ok: true };
  }

  if (kind === 'RejectEnvelope') {
    if (!hasKeys(value, ['abi_version', 'canonical_evidence', 'reject_code', 'reject_kind', 'stage'])) {
      return abiReject('missing_field', { kind });
    }
    if (!REJECT_KIND.includes(value.reject_kind)) {
      return abiReject('invalid_reject_kind', { kind, value: value.reject_kind });
    }
    if (!(value.stage === null || STAGE_KIND.includes(value.stage))) {
      return abiReject('invalid_stage', { kind, value: value.stage });
    }
    return { ok: true };
  }

  if (kind === 'SIDOutput') {
    if (!hasKeys(value, ['derivation_context', 'digest', 'federation_scope', 'schema_digest', 'sid'])) {
      return abiReject('missing_field', { kind });
    }
    if (!/^[0-9a-f]{64}$/.test(value.digest) || !isSha256(value.schema_digest) || !isSid(value.sid)) {
      return abiReject('invalid_identity_shape', { kind });
    }
    return { ok: true };
  }

  if (kind === 'IdentityDescriptor') {
    if (!hasKeys(value, ['adapters', 'descriptor_digest', 'epoch', 'federation_scope', 'governors', 'revocation', 'schema_digest', 'services', 'sid', 'spec'])) {
      return abiReject('missing_field', { kind });
    }
    if (!isSha256(value.descriptor_digest) || !isSha256(value.schema_digest) || !isSid(value.sid)) {
      return abiReject('invalid_descriptor_shape', { kind });
    }
    return { ok: true };
  }

  if (kind === 'DescriptorVerificationResult') {
    if (!hasKeys(value, ['abi_version', 'descriptor_digest', 'sid', 'status', 'evidence'])) {
      return abiReject('missing_field', { kind });
    }
    if (!DESCRIPTOR_STATUS.includes(value.status)) {
      return abiReject('invalid_status', { kind, value: value.status });
    }
    return { ok: true };
  }

  if (kind === 'CapabilityVerificationResult') {
    if (!hasKeys(value, ['abi_version', 'capability_digest', 'status', 'evidence'])) {
      return abiReject('missing_field', { kind });
    }
    if (!CAPABILITY_STATUS.includes(value.status)) {
      return abiReject('invalid_status', { kind, value: value.status });
    }
    if (!isSha256(value.capability_digest)) {
      return abiReject('invalid_digest', { kind });
    }
    return { ok: true };
  }

  if (kind === 'RevocationRecord') {
    if (!hasKeys(value, ['capability_digest', 'epoch', 'governor_sid', 'reason', 'revocation_digest', 'subject_sid'])) {
      return abiReject('missing_field', { kind });
    }
    if (!isSha256(value.capability_digest) || !isSha256(value.revocation_digest)) {
      return abiReject('invalid_digest', { kind });
    }
    if (!isSid(value.governor_sid) || !isSid(value.subject_sid)) {
      return abiReject('invalid_sid', { kind });
    }
    return { ok: true };
  }

  if (kind === 'RevocationVerificationResult') {
    if (!hasKeys(value, ['abi_version', 'capability_digest', 'status', 'evidence'])) {
      return abiReject('missing_field', { kind });
    }
    if (!REVOCATION_STATUS.includes(value.status)) {
      return abiReject('invalid_status', { kind, value: value.status });
    }
    return { ok: true };
  }

  if (kind === 'EVREvent') {
    if (!hasKeys(value, ['actor_sid', 'causal', 'event_id', 'event_kind', 'event_version', 'federation_scope', 'node_sid', 'payload', 'schema_digest', 'subject_sid', 'timestamp_epoch'])) {
      return abiReject('missing_field', { kind });
    }
    if (value.event_version !== 'EVR-1.0') {
      return abiReject('invalid_event_version', { kind, value: value.event_version });
    }
    if (!knownEventKind(value.event_kind)) {
      return abiReject('invalid_event_kind', { kind, value: value.event_kind });
    }
    const required = requiredEvidenceFieldsForKind(value.event_kind) ?? [];
    for (const field of required) {
      if (!Object.prototype.hasOwnProperty.call(value.causal, field)) {
        return abiReject('missing_evidence_field', { kind, event_kind: value.event_kind, field });
      }
    }
    return { ok: true };
  }

  if (kind === 'FederationAnnouncement') {
    if (!hasKeys(value, ['descriptor_digest', 'epoch', 'federation_scope', 'node_sid', 'peer_endpoint', 'schema_digest'])) {
      return abiReject('missing_field', { kind });
    }
    if (!isSha256(value.descriptor_digest) || !isSha256(value.schema_digest)) {
      return abiReject('invalid_digest', { kind });
    }
    if (!isSid(value.node_sid)) {
      return abiReject('invalid_sid', { kind });
    }
    return { ok: true };
  }

  if (kind === 'RouteSet') {
    if (!hasKeys(value, ['entries', 'epoch', 'federation_scope'])) {
      return abiReject('missing_field', { kind });
    }
    return { ok: true };
  }

  if (kind === 'ArbitrationResult') {
    if (!hasKeys(value, ['abi_version', 'call_digest', 'status', 'evidence'])) {
      return abiReject('missing_field', { kind });
    }
    if (!ARBITRATION_STATUS.includes(value.status)) {
      return abiReject('invalid_status', { kind, value: value.status });
    }
    return { ok: true };
  }

  if (kind === 'ConvergenceWitness') {
    if (!hasKeys(value, ['call_digest', 'result_digest', 'runtime_sids', 'stage', 'test_id'])) {
      return abiReject('missing_field', { kind });
    }
    const sorted = [...value.runtime_sids].sort();
    if (JSON.stringify(sorted) !== JSON.stringify(value.runtime_sids)) {
      return abiReject('invalid_ordering', { field: 'runtime_sids', kind });
    }
    return { ok: true };
  }

  if (kind === 'DivergenceWitness') {
    if (!hasKeys(value, ['call_digest', 'divergence_stage', 'result_digests', 'runtime_sids', 'test_id'])) {
      return abiReject('missing_field', { kind });
    }
    const sorted = [...value.runtime_sids].sort();
    if (JSON.stringify(sorted) !== JSON.stringify(value.runtime_sids)) {
      return abiReject('invalid_ordering', { field: 'runtime_sids', kind });
    }
    return { ok: true };
  }

  if (kind === 'AdapterDerivationResult') {
    if (!hasKeys(value, ['abi_version', 'status', 'evidence'])) {
      return abiReject('missing_field', { kind });
    }
    if (!ADAPTER_STATUS.includes(value.status)) {
      return abiReject('invalid_status', { kind, value: value.status });
    }
    return { ok: true };
  }

  return abiReject('unknown_structure', { kind });
}
