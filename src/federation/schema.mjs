import { createHash } from 'node:crypto';

import { STAGES, canonicalJson } from '../protocol/dbc.mjs';

const SID_RE = /^sid:dbc:[0-9a-f]{64}$/;
const SHA256_RE = /^sha256:[0-9a-f]{64}$/;

function sha256hex(text) {
  return createHash('sha256').update(Buffer.from(text, 'utf8')).digest('hex');
}

function uniqueSortedStrings(values) {
  return [...new Set((values ?? []).filter((v) => typeof v === 'string' && v.length > 0))].sort();
}

function reject(code, evidence = {}) {
  return {
    code,
    evidence,
    kind: 'FederationValidationFailure',
    ok: false,
  };
}

function canonicalizeDescriptor(descriptor) {
  return {
    endpoint: descriptor?.endpoint,
    epoch: {
      not_after: descriptor?.epoch?.not_after,
      not_before: descriptor?.epoch?.not_before,
    },
    federation_scope: descriptor?.federation_scope ?? '',
    provider_id: descriptor?.provider_id,
    schema_digests: uniqueSortedStrings(descriptor?.schema_digests),
    supported_stages: uniqueSortedStrings(descriptor?.supported_stages),
    version: descriptor?.version ?? 'federation/v1',
    priority: Number.isInteger(descriptor?.priority) ? descriptor.priority : 0,
  };
}

export function computeFederationDescriptorDigest(descriptor) {
  return `sha256:${sha256hex(canonicalJson(canonicalizeDescriptor(descriptor)))}`;
}

export function validateFederationDescriptor(descriptor, { now_epoch = 0 } = {}) {
  const normalized = canonicalizeDescriptor(descriptor);

  if (normalized.version !== 'federation/v1') {
    return reject('invalid_federation_descriptor', { reason: 'unsupported_version', version: normalized.version });
  }
  if (typeof normalized.provider_id !== 'string' || normalized.provider_id.length === 0) {
    return reject('invalid_federation_descriptor', { reason: 'provider_id_required' });
  }
  if (typeof normalized.endpoint !== 'string' || normalized.endpoint.length === 0) {
    return reject('invalid_federation_descriptor', { reason: 'endpoint_required' });
  }

  if (!Number.isInteger(normalized.epoch.not_before) || !Number.isInteger(normalized.epoch.not_after) || normalized.epoch.not_before > normalized.epoch.not_after) {
    return reject('invalid_federation_descriptor', { reason: 'invalid_epoch' });
  }
  if (now_epoch < normalized.epoch.not_before) {
    return reject('invalid_federation_descriptor', { reason: 'descriptor_not_yet_valid' });
  }
  if (now_epoch > normalized.epoch.not_after) {
    return reject('invalid_federation_descriptor', { reason: 'descriptor_expired' });
  }

  if (normalized.supported_stages.length === 0 || !normalized.supported_stages.every((s) => STAGES.includes(s))) {
    return reject('invalid_federation_descriptor', { reason: 'invalid_supported_stages', supported_stages: normalized.supported_stages });
  }
  if (normalized.schema_digests.length === 0 || !normalized.schema_digests.every((d) => SHA256_RE.test(d))) {
    return reject('invalid_federation_descriptor', { reason: 'invalid_schema_digests', schema_digests: normalized.schema_digests });
  }

  const expectedDigest = computeFederationDescriptorDigest(normalized);
  if (typeof descriptor?.descriptor_digest === 'string' && descriptor.descriptor_digest !== expectedDigest) {
    return reject('invalid_federation_descriptor', { reason: 'descriptor_digest_mismatch', expected: expectedDigest, got: descriptor.descriptor_digest });
  }

  return {
    ok: true,
    value: {
      ...normalized,
      descriptor_digest: expectedDigest,
    },
  };
}

export function validateAnnouncement(announcement, options = {}) {
  if (!announcement || typeof announcement !== 'object' || Array.isArray(announcement)) {
    return reject('malformed_announcement', { reason: 'announcement_object_required' });
  }

  const descriptor = validateFederationDescriptor(announcement.descriptor, options);
  if (!descriptor.ok) {
    return descriptor;
  }

  if (typeof announcement.provider_id !== 'string' || announcement.provider_id !== descriptor.value.provider_id) {
    return reject('malformed_announcement', {
      reason: 'provider_id_mismatch',
      expected_provider_id: descriptor.value.provider_id,
      got_provider_id: announcement.provider_id ?? null,
    });
  }

  return {
    ok: true,
    value: {
      announcement_id: `fan:${sha256hex(canonicalJson({
        descriptor_digest: descriptor.value.descriptor_digest,
        provider_id: descriptor.value.provider_id,
      }))}`,
      descriptor: descriptor.value,
      provider_id: descriptor.value.provider_id,
    },
  };
}

export function validateFederationRequest(request) {
  if (!request || typeof request !== 'object' || Array.isArray(request)) {
    return reject('invalid_request', { reason: 'request_object_required' });
  }

  if (typeof request.sid !== 'string' || !SID_RE.test(request.sid)) {
    return reject('invalid_request', { reason: 'invalid_sid', sid: request.sid ?? null });
  }
  if (typeof request.stage !== 'string' || !STAGES.includes(request.stage)) {
    return reject('invalid_request', { reason: 'invalid_stage', stage: request.stage ?? null });
  }
  if (typeof request.schema_digest !== 'string' || !SHA256_RE.test(request.schema_digest)) {
    return reject('invalid_request', { reason: 'invalid_schema_digest', schema_digest: request.schema_digest ?? null });
  }
  if (typeof request.federation_scope !== 'string') {
    return reject('invalid_request', { reason: 'invalid_federation_scope' });
  }

  return {
    ok: true,
    value: {
      federation_scope: request.federation_scope,
      schema_digest: request.schema_digest,
      sid: request.sid,
      stage: request.stage,
    },
  };
}
