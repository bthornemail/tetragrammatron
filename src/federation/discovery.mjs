import { canonicalJson } from '../protocol/dbc.mjs';
import { validateAnnouncement, validateFederationRequest } from './schema.mjs';

function reject(code, evidence = {}) {
  return {
    code,
    evidence,
    kind: 'FederationDiscoveryFailure',
    ok: false,
  };
}

function isScopeCompatible(requestScope, descriptorScope) {
  if (descriptorScope === '') {
    return true;
  }
  return requestScope === descriptorScope;
}

function candidateFromAnnouncement(announcement) {
  return {
    descriptor_digest: announcement.descriptor.descriptor_digest,
    endpoint: announcement.descriptor.endpoint,
    federation_scope: announcement.descriptor.federation_scope,
    priority: announcement.descriptor.priority,
    provider_id: announcement.provider_id,
    schema_digests: announcement.descriptor.schema_digests,
    supported_stages: announcement.descriptor.supported_stages,
  };
}

function canonicalCandidateSort(a, b) {
  return canonicalJson({
    descriptor_digest: a.descriptor_digest,
    endpoint: a.endpoint,
    federation_scope: a.federation_scope,
    priority: a.priority,
    provider_id: a.provider_id,
  }).localeCompare(canonicalJson({
    descriptor_digest: b.descriptor_digest,
    endpoint: b.endpoint,
    federation_scope: b.federation_scope,
    priority: b.priority,
    provider_id: b.provider_id,
  }));
}

export function deriveRouteSet(announcements, request, options = {}) {
  if (!Array.isArray(announcements) || announcements.length === 0) {
    return reject('invalid_request', { reason: 'announcements_required' });
  }

  const normalizedReq = validateFederationRequest(request);
  if (!normalizedReq.ok) {
    return normalizedReq;
  }

  const accepted = [];
  const seenByProvider = new Map();

  for (const item of announcements) {
    const valid = validateAnnouncement(item, options);
    if (!valid.ok) {
      return valid;
    }

    const existing = seenByProvider.get(valid.value.provider_id);
    if (existing && existing.descriptor.descriptor_digest !== valid.value.descriptor.descriptor_digest) {
      return reject('announcement_conflict', {
        descriptor_a: existing.descriptor.descriptor_digest,
        descriptor_b: valid.value.descriptor.descriptor_digest,
        provider_id: valid.value.provider_id,
      });
    }

    seenByProvider.set(valid.value.provider_id, valid.value);
  }

  for (const announcement of seenByProvider.values()) {
    const d = announcement.descriptor;
    if (!d.supported_stages.includes(normalizedReq.value.stage)) {
      continue;
    }
    if (!d.schema_digests.includes(normalizedReq.value.schema_digest)) {
      continue;
    }
    if (!isScopeCompatible(normalizedReq.value.federation_scope, d.federation_scope)) {
      continue;
    }
    accepted.push(candidateFromAnnouncement(announcement));
  }

  accepted.sort(canonicalCandidateSort);

  if (accepted.length === 0) {
    return reject('route_unreachable', {
      federation_scope: normalizedReq.value.federation_scope,
      schema_digest: normalizedReq.value.schema_digest,
      sid: normalizedReq.value.sid,
      stage: normalizedReq.value.stage,
    });
  }

  return {
    ok: true,
    value: {
      candidates: accepted,
      request: normalizedReq.value,
    },
  };
}
