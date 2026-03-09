import { createHash } from 'node:crypto';

import { canonicalJson } from '../protocol/dbc.mjs';
import { revocationReject } from './errors.mjs';
import { validateRevocationRecord } from './validate.mjs';

function digestChain(chain) {
  return `ch:${createHash('sha256').update(Buffer.from(chain.map((g) => g.grant_id).join('|'), 'utf8')).digest('hex')}`;
}

function stableDigest(value) {
  return `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(value), 'utf8')).digest('hex')}`;
}

function isInScope(recordScope, request) {
  const req = request ?? {};

  if (typeof req.action === 'string' && recordScope.actions.length > 0 && !recordScope.actions.includes(req.action)) {
    return false;
  }

  if (typeof req.resource === 'string' && recordScope.resources.length > 0 && !recordScope.resources.includes(req.resource)) {
    return false;
  }

  if (typeof req.adapter_label === 'string' && recordScope.adapters.length > 0 && !recordScope.adapters.includes(req.adapter_label)) {
    return false;
  }

  return true;
}

function sortRecords(records) {
  return [...records].sort((a, b) => {
    const byId = a.record_id.localeCompare(b.record_id);
    if (byId !== 0) {
      return byId;
    }
    return a.target_ref.localeCompare(b.target_ref);
  });
}

function isAuthorizedRevoker({ record, rootGrant, targetGrant, trustAnchors }) {
  if (trustAnchors.includes(record.revoker_id)) {
    return true;
  }

  if (rootGrant && record.revoker_id === rootGrant.governor_sid) {
    return true;
  }

  if (record.target_kind === 'grant' && targetGrant && record.revoker_id === targetGrant.governor_sid) {
    return true;
  }

  return false;
}

export function verifyRevocationSet(input) {
  const chain = input?.capability_chain;
  const revocations = Array.isArray(input?.revocation_records) ? input.revocation_records : [];
  const trustAnchors = [...new Set((input?.trust_anchors ?? []).filter((v) => typeof v === 'string'))].sort();
  const nowEpoch = Number.isInteger(input?.now_epoch) ? input.now_epoch : 0;
  const request = input?.request ?? {};

  if (!Array.isArray(chain) || chain.length === 0) {
    return revocationReject('target_not_found', { reason: 'capability_chain_required' });
  }

  if (revocations.length === 0) {
    return {
      ok: true,
      status: 'not_revoked',
      evidence: {
        checked_count: 0,
        revocation_set_digest: stableDigest([]),
      },
    };
  }

  const chainDigest = digestChain(chain);
  const rootGrant = chain[0];
  const grantsById = new Map(chain.map((grant, index) => [grant.grant_id, { grant, index }]));

  const parsed = [];
  for (let i = 0; i < revocations.length; i += 1) {
    const checked = validateRevocationRecord(revocations[i], i);
    if (!checked.ok) {
      return checked;
    }
    parsed.push(checked.value);
  }

  const ordered = sortRecords(parsed);

  for (const record of ordered) {
    let targetGrant = null;

    if (record.target_kind === 'grant') {
      const located = grantsById.get(record.target_ref);
      if (!located) {
        return revocationReject('target_not_found', {
          record_id: record.record_id,
          target_kind: record.target_kind,
          target_ref: record.target_ref,
        });
      }
      targetGrant = located.grant;
    }

    if (record.target_kind === 'chain' && record.target_ref !== chainDigest) {
      return revocationReject('target_not_found', {
        chain_digest: chainDigest,
        record_id: record.record_id,
        target_kind: record.target_kind,
        target_ref: record.target_ref,
      });
    }

    if (!isAuthorizedRevoker({ record, rootGrant, targetGrant, trustAnchors })) {
      return revocationReject('unauthorized_revoker', {
        record_id: record.record_id,
        revoker_id: record.revoker_id,
        trust_anchors: trustAnchors,
      });
    }

    if (nowEpoch < record.effective_epoch) {
      return revocationReject('revocation_out_of_scope', {
        now_epoch: nowEpoch,
        reason: 'effective_epoch_not_reached',
        record_id: record.record_id,
      });
    }

    if (!isInScope(record.scope, request)) {
      return revocationReject('revocation_out_of_scope', {
        record_id: record.record_id,
        reason: 'request_outside_revocation_scope',
      });
    }

    return {
      ok: false,
      status: 'revoked',
      evidence: {
        chain_digest: chainDigest,
        record_id: record.record_id,
        revocation_set_digest: stableDigest(ordered.map((r) => r.record_id)),
        revoker_id: record.revoker_id,
        target_kind: record.target_kind,
        target_ref: record.target_ref,
      },
    };
  }

  return {
    ok: true,
    status: 'not_revoked',
    evidence: {
      chain_digest: chainDigest,
      checked_count: ordered.length,
      revocation_set_digest: stableDigest(ordered.map((r) => r.record_id)),
    },
  };
}
