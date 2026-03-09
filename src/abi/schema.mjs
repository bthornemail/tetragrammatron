import { createHash } from 'node:crypto';

import { canonicalJson } from '../protocol/dbc.mjs';
import { ABI_EVENT_VERSION } from './kinds.mjs';

export const ABI_VERSION = 'ABI-1.1.0';
export const STAGE_KIND = ['Realized', 'Closed', 'Normalized', 'Projected'];
export const REJECT_KIND = ['RejectRealize', 'RejectClose', 'RejectNormalize', 'RejectProject'];
export const DESCRIPTOR_STATUS = ['valid', 'digest_mismatch', 'epoch_invalid', 'schema_mismatch', 'not_found'];
export const CAPABILITY_STATUS = ['valid', 'expired', 'revoked', 'invalid_signature', 'broken_chain', 'schema_mismatch'];
export const REVOCATION_STATUS = ['not_revoked', 'revoked', 'record_invalid', 'record_not_found'];
export const ADAPTER_STATUS = ['derived', 'denied', 'unknown'];
export const ARBITRATION_STATUS = ['converged', 'diverged', 'inconclusive'];

const NULL_SID = 'sid:dbc:0000000000000000000000000000000000000000000000000000000000000000';

function sha256(value) {
  return `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(value), 'utf8')).digest('hex')}`;
}

function sortedArray(values, mapper = (v) => v) {
  return [...values].sort((a, b) => mapper(a).localeCompare(mapper(b)));
}

export function normalizeNormalForm(value) {
  return {
    constraints: sortedArray(value?.constraints ?? [], (v) => canonicalJson(v)),
    edges: sortedArray(value?.edges ?? [], (v) => canonicalJson(v)),
    nodes: sortedArray(value?.nodes ?? [], (v) => String(v)),
    witnesses: sortedArray(value?.witnesses ?? [], (v) => canonicalJson(v)),
  };
}

export function toResolveCallAbi(call) {
  const canonicalInput = {
    d: call?.canonical_input?.document ?? [],
  };
  return {
    abi_version: ABI_VERSION,
    canonical_input: canonicalInput,
    schema_digest: call?.canonical_input?.schema ? sha256(call.canonical_input.schema) : call?.schema_digest ?? null,
    target_stage: call?.target_stage ?? null,
    view_digest: call?.canonical_input?.view ? sha256(call.canonical_input.view) : null,
  };
}

export function toResolveResultAbi(result, call = null) {
  const canonicalValue = result?.value_kind === 'NormalForm'
    ? normalizeNormalForm(result.value)
    : (result?.value ?? null);
  return {
    abi_version: ABI_VERSION,
    canonical_value: canonicalValue,
    schema_digest: result?.identity?.schema_digest ?? (call?.canonical_input?.schema ? sha256(call.canonical_input.schema) : null),
    stage: result?.stage ?? null,
    value_digest: sha256(canonicalValue),
    value_kind: result?.value_kind ?? null,
  };
}

export function toRejectEnvelopeAbi(result) {
  return {
    abi_version: ABI_VERSION,
    canonical_evidence: result?.evidence ?? {},
    reject_code: result?.reject_code ?? (result?.code ?? 'reject'),
    reject_kind: result?.reject_kind ?? null,
    stage: result?.stage ?? null,
  };
}

export function toSIDOutputAbi({ sid, digest, schema_digest, derivation_context = null, federation_scope = null }) {
  return {
    derivation_context: derivation_context ?? null,
    digest,
    federation_scope: federation_scope ?? null,
    schema_digest,
    sid,
  };
}

export function toIdentityDescriptorAbi(descriptor) {
  const base = {
    adapters: descriptor?.adapters ?? {},
    descriptor_digest: descriptor?.descriptor_digest ?? null,
    epoch: descriptor?.epoch ?? '0',
    federation_scope: descriptor?.federation_scope ?? null,
    governors: sortedArray(descriptor?.governors ?? [], (v) => canonicalJson(v)),
    revocation: descriptor?.revocation ?? {},
    schema_digest: descriptor?.schema_digest ?? null,
    services: sortedArray(descriptor?.services ?? [], (v) => String(v?.id ?? canonicalJson(v))),
    sid: descriptor?.sid ?? null,
    spec: descriptor?.spec ?? 'DBC-IDL-1.2',
  };
  const digest = sha256({ ...base, descriptor_digest: null });
  return {
    ...base,
    descriptor_digest: base.descriptor_digest ?? digest,
  };
}

export function toDescriptorVerificationResultAbi({ descriptor, sid, status, evidence = {} }) {
  return {
    abi_version: ABI_VERSION,
    descriptor_digest: descriptor?.descriptor_digest ?? null,
    sid,
    status,
    evidence,
  };
}

function mapCapabilityStatus(status, ok) {
  if (ok) {
    return 'valid';
  }
  if (status === 'epoch_expired') {
    return 'expired';
  }
  if (status === 'revoked') {
    return 'revoked';
  }
  if (status === 'invalid_signature') {
    return 'invalid_signature';
  }
  if (status === 'malformed_capability' || status === 'unsupported_capability_version' || status === 'invalid_request') {
    return 'schema_mismatch';
  }
  return 'broken_chain';
}

export function toCapabilityVerificationResultAbi(result, input = {}) {
  const chain = input?.capability_chain ?? [];
  const capabilityDigest = sha256(chain);
  return {
    abi_version: ABI_VERSION,
    capability_digest: capabilityDigest,
    status: mapCapabilityStatus(result?.status, result?.ok),
    evidence: {
      original_status: result?.status ?? null,
      ...(result?.evidence ?? {}),
    },
  };
}

function mapRevocationStatus(status) {
  if (status === 'not_revoked' || status === 'revoked') {
    return status;
  }
  if (status === 'target_not_found') {
    return 'record_not_found';
  }
  return 'record_invalid';
}

export function toRevocationRecordAbi(record) {
  return {
    capability_digest: record?.target_ref?.startsWith('cg:') ? `sha256:${record.target_ref.slice(3)}` : sha256(record?.target_ref ?? null),
    epoch: String(record?.effective_epoch ?? '0'),
    governor_sid: record?.revoker_id ?? NULL_SID,
    reason: record?.reason ?? null,
    revocation_digest: record?.record_id?.startsWith('cr:') ? `sha256:${record.record_id.slice(3)}` : sha256(record),
    subject_sid: record?.subject_sid ?? NULL_SID,
  };
}

export function toRevocationVerificationResultAbi(result, input = {}) {
  const chain = input?.capability_chain ?? [];
  return {
    abi_version: ABI_VERSION,
    capability_digest: sha256(chain),
    status: mapRevocationStatus(result?.status ?? 'record_invalid'),
    evidence: {
      original_status: result?.status ?? null,
      ...(result?.evidence ?? {}),
    },
  };
}

export function toEVREventAbi(event) {
  const sid = event?.evidence?.sid ?? event?.evidence?.subject_sid ?? null;
  return {
    actor_sid: event?.evidence?.actor_sid ?? null,
    causal: event?.evidence ?? {},
    event_id: event?.event_id ?? null,
    event_kind: event?.kind ?? null,
    event_version: ABI_EVENT_VERSION,
    federation_scope: event?.evidence?.federation_scope ?? null,
    node_sid: event?.evidence?.node_sid ?? sid ?? NULL_SID,
    payload: {
      family: event?.family ?? null,
      origin_layer: event?.origin_layer ?? null,
      status: event?.status ?? null,
    },
    schema_digest: event?.evidence?.schema_digest ?? null,
    subject_sid: sid,
    timestamp_epoch: event?.timestamp ?? null,
  };
}

export function toFederationAnnouncementAbi(announcement) {
  const desc = announcement?.descriptor ?? {};
  return {
    descriptor_digest: desc.descriptor_digest ?? null,
    epoch: String(announcement?.announcement_epoch ?? 0),
    federation_scope: desc.federation_scope ?? '',
    node_sid: desc.node_sid ?? NULL_SID,
    peer_endpoint: desc.endpoint ?? '',
    schema_digest: (desc.schema_digests ?? [null])[0],
  };
}

export function toRouteSetAbi(routeSet) {
  const entries = sortedArray(routeSet?.candidates ?? [], (v) => String(v.sid ?? '')).map((entry) => ({
    ipv6_address: entry.adapter?.credential ?? null,
    liveness: 'unknown',
    peer_endpoint: entry.endpoint,
    sid: entry.sid,
    transport: 'hd-rpc',
  }));
  return {
    entries,
    epoch: String(routeSet?.request?.now_epoch ?? 0),
    federation_scope: routeSet?.request?.federation_scope ?? '',
  };
}

export function toArbitrationResultAbi(arbitration, callDigest = null) {
  return {
    abi_version: ABI_VERSION,
    call_digest: callDigest ?? sha256(arbitration?.request ?? {}),
    status: arbitration?.selected ? 'converged' : 'inconclusive',
    evidence: arbitration ?? {},
  };
}

export function toConvergenceWitnessAbi(witness) {
  return {
    call_digest: witness?.call_digest ?? null,
    result_digest: witness?.result_digest ?? null,
    runtime_sids: sortedArray(witness?.runtime_sids ?? [], (v) => String(v)),
    stage: witness?.stage ?? null,
    test_id: witness?.test_id ?? null,
  };
}

export function toDivergenceWitnessAbi(witness) {
  return {
    call_digest: witness?.call_digest ?? null,
    divergence_stage: witness?.divergence_stage ?? null,
    result_digests: witness?.result_digests ?? {},
    runtime_sids: sortedArray(witness?.runtime_sids ?? [], (v) => String(v)),
    test_id: witness?.test_id ?? null,
  };
}
