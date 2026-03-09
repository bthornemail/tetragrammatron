import { createHash } from 'node:crypto';

import { canonicalJson } from './dbc.mjs';
import { verifyRevocationSet } from '../revocation/verify.mjs';

const CAPABILITY_VERSION = 'capability/v1';
const SID_RE = /^sid:dbc:[0-9a-f]{64}$/;

function sha256hex(input) {
  return createHash('sha256').update(Buffer.from(input, 'utf8')).digest('hex');
}

function uniqueSorted(values) {
  return [...new Set((values ?? []).filter((v) => typeof v === 'string'))].sort();
}

function isSid(value) {
  return typeof value === 'string' && SID_RE.test(value);
}

function reject(status, evidence = {}) {
  return {
    ok: false,
    status,
    evidence,
  };
}

export function canonicalizeScope(scope) {
  return {
    actions: uniqueSorted(scope?.actions),
    adapters: uniqueSorted(scope?.adapters),
    resources: uniqueSorted(scope?.resources),
  };
}

function isSubset(child, parent) {
  const parentSet = new Set(parent);
  return child.every((item) => parentSet.has(item));
}

function canonicalGrantPayload(grant) {
  return {
    epoch: {
      not_after: grant.epoch.not_after,
      not_before: grant.epoch.not_before,
    },
    governor_sid: grant.governor_sid,
    parent_grant_id: grant.parent_grant_id,
    scope: canonicalizeScope(grant.scope),
    subject_sid: grant.subject_sid,
    actor_sid: grant.actor_sid,
    version: grant.version,
  };
}

export function computeGrantId(grant) {
  return `cg:${sha256hex(canonicalJson(canonicalGrantPayload(grant)))}`;
}

function computeSignature(grant, bySid) {
  const payload = canonicalJson({
    by_sid: bySid,
    grant_id: computeGrantId(grant),
  });
  return `sig:${sha256hex(`capability-signature-v1\0${payload}`)}`;
}

export function signGrant(grant, bySid = grant.governor_sid) {
  const normalized = normalizeGrant(grant);
  return {
    ...normalized,
    grant_id: computeGrantId(normalized),
    signature: {
      by_sid: bySid,
      sig: computeSignature(normalized, bySid),
    },
  };
}

function normalizeGrant(grant) {
  const epoch = (grant && typeof grant === 'object' && grant.epoch && typeof grant.epoch === 'object')
    ? grant.epoch
    : {};
  return {
    actor_sid: grant?.actor_sid,
    epoch: {
      not_after: epoch.not_after,
      not_before: epoch.not_before,
    },
    governor_sid: grant?.governor_sid,
    parent_grant_id: grant?.parent_grant_id ?? null,
    scope: canonicalizeScope(grant?.scope),
    subject_sid: grant?.subject_sid,
    version: grant?.version ?? CAPABILITY_VERSION,
  };
}

function validateGrantShape(grant, index) {
  if (!grant || typeof grant !== 'object') {
    return reject('malformed_capability', { index, reason: 'grant_not_object' });
  }

  const normalized = normalizeGrant(grant);

  if (normalized.version !== CAPABILITY_VERSION) {
    return reject('unsupported_capability_version', { index, version: normalized.version });
  }

  if (!isSid(normalized.governor_sid) || !isSid(normalized.subject_sid) || !isSid(normalized.actor_sid)) {
    return reject('malformed_capability', { index, reason: 'invalid_sid_shape' });
  }

  const epoch = normalized.epoch;
  if (!Number.isInteger(epoch.not_before) || !Number.isInteger(epoch.not_after) || epoch.not_before > epoch.not_after) {
    return reject('malformed_capability', { index, reason: 'invalid_epoch' });
  }

  return { ok: true, value: normalized };
}

function verifyGrantSignature(grant) {
  const signature = grant.signature;
  if (!signature || typeof signature !== 'object') {
    return false;
  }
  if (!isSid(signature.by_sid) || signature.by_sid !== grant.governor_sid) {
    return false;
  }
  return signature.sig === computeSignature(grant, signature.by_sid);
}

export function verifyCapabilityChain(input) {
  const chain = input?.capability_chain;
  const revocationRecords = Array.isArray(input?.revocation_records) ? input.revocation_records : [];
  const trustAnchors = uniqueSorted(input?.trust_anchors);
  const nowEpoch = Number.isInteger(input?.now_epoch) ? input.now_epoch : 0;
  const request = input?.request ?? {};
  const maxDepth = Number.isInteger(input?.max_delegation_depth) ? input.max_delegation_depth : 8;

  if (!Array.isArray(chain) || chain.length === 0) {
    return reject('invalid_request', { reason: 'capability_chain_required' });
  }
  if (trustAnchors.length === 0) {
    return reject('invalid_request', { reason: 'trust_anchors_required' });
  }

  if (chain.length > maxDepth) {
    return reject('unauthorized_delegation_depth', { chain_length: chain.length, max_delegation_depth: maxDepth });
  }

  const normalizedChain = [];
  for (let i = 0; i < chain.length; i += 1) {
    const shape = validateGrantShape(chain[i], i);
    if (!shape.ok) {
      return shape;
    }

    const grant = {
      ...shape.value,
      grant_id: chain[i].grant_id ?? computeGrantId(shape.value),
      signature: chain[i].signature,
    };

    if (grant.grant_id !== computeGrantId(grant)) {
      return reject('broken_chain', { index: i, reason: 'grant_id_mismatch' });
    }

    if (!verifyGrantSignature(grant)) {
      return reject('invalid_signature', { index: i, grant_id: grant.grant_id });
    }

    if (nowEpoch < grant.epoch.not_before) {
      return reject('epoch_not_yet_valid', { index: i, now_epoch: nowEpoch, not_before: grant.epoch.not_before });
    }

    if (nowEpoch > grant.epoch.not_after) {
      return reject('epoch_expired', { index: i, now_epoch: nowEpoch, not_after: grant.epoch.not_after });
    }

    normalizedChain.push(grant);
  }

  const root = normalizedChain[0];
  if (root.parent_grant_id !== null) {
    return reject('broken_chain', { index: 0, reason: 'root_parent_must_be_null' });
  }
  if (!trustAnchors.includes(root.governor_sid)) {
    return reject('invalid_governor', { governor_sid: root.governor_sid, trust_anchors: trustAnchors });
  }

  for (let i = 1; i < normalizedChain.length; i += 1) {
    const prev = normalizedChain[i - 1];
    const cur = normalizedChain[i];

    if (cur.parent_grant_id !== prev.grant_id) {
      return reject('broken_chain', { index: i, reason: 'parent_link_mismatch' });
    }

    if (cur.governor_sid !== prev.actor_sid) {
      return reject('wrong_governor', {
        expected_governor_sid: prev.actor_sid,
        got_governor_sid: cur.governor_sid,
        index: i,
      });
    }

    if (cur.subject_sid !== prev.subject_sid) {
      return reject('identity_mismatch', { index: i, reason: 'subject_sid_mismatch' });
    }

    if (!isSubset(cur.scope.actions, prev.scope.actions) || !isSubset(cur.scope.resources, prev.scope.resources) || !isSubset(cur.scope.adapters, prev.scope.adapters)) {
      return reject('scope_escalation', { index: i });
    }
  }

  const finalGrant = normalizedChain[normalizedChain.length - 1];

  if (request.subject_sid && request.subject_sid !== finalGrant.subject_sid) {
    return reject('identity_mismatch', {
      expected_subject_sid: finalGrant.subject_sid,
      got_subject_sid: request.subject_sid,
    });
  }

  if (request.actor_sid && request.actor_sid !== finalGrant.actor_sid) {
    return reject('scope_mismatch', {
      reason: 'actor_not_authorized',
      expected_actor_sid: finalGrant.actor_sid,
      got_actor_sid: request.actor_sid,
    });
  }

  if (request.action && !finalGrant.scope.actions.includes(request.action)) {
    return reject('scope_mismatch', { reason: 'action_not_allowed', action: request.action });
  }

  if (request.resource && !finalGrant.scope.resources.includes(request.resource)) {
    return reject('scope_mismatch', { reason: 'resource_not_allowed', resource: request.resource });
  }

  if (request.adapter_label && !finalGrant.scope.adapters.includes(request.adapter_label)) {
    return reject('adapter_not_authorized', { adapter_label: request.adapter_label });
  }

  const revocation = verifyRevocationSet({
    capability_chain: normalizedChain,
    now_epoch: nowEpoch,
    request,
    revocation_records: revocationRecords,
    trust_anchors: trustAnchors,
  });
  if (!revocation.ok) {
    return reject(revocation.status, {
      ...revocation.evidence,
      revocation_status: revocation.status,
    });
  }

  return {
    ok: true,
    status: 'verified',
    evidence: {
      actor_sid: finalGrant.actor_sid,
      chain_digest: `ch:${sha256hex(normalizedChain.map((g) => g.grant_id).join('|'))}`,
      chain_length: normalizedChain.length,
      revocation_checked: true,
      revocation_set_digest: revocation.evidence.revocation_set_digest,
      root_governor_sid: root.governor_sid,
      subject_sid: finalGrant.subject_sid,
    },
  };
}
