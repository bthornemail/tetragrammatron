import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { signRevocationRecord } from '../../src/revocation/schema.mjs';
import { SIDS, buildValidDelegated, buildValidSingle } from '../capability/fixture.mjs';

export async function loadRevocationCases(kind) {
  const p = path.join(process.cwd(), 'fixtures', 'revocation', kind, 'cases.json');
  const raw = await readFile(p, 'utf8');
  return JSON.parse(raw).cases;
}

export function chainDigest(chain) {
  return `ch:${createHash('sha256').update(Buffer.from(chain.map((g) => g.grant_id).join('|'), 'utf8')).digest('hex')}`;
}

export function makeGrantRevocation({ chain, grantId, revokerId = SIDS.govRoot, effectiveEpoch = 20, scope } = {}) {
  return signRevocationRecord({
    effective_epoch: effectiveEpoch,
    revoker_id: revokerId,
    scope: scope ?? {
      actions: ['derive_adapter', 'resolve'],
      adapters: ['adapter:guarded-demo'],
      resources: ['resource:alpha'],
    },
    target_kind: 'grant',
    target_ref: grantId ?? chain[0].grant_id,
    version: 'revocation/v1',
  });
}

export function makeChainRevocation({ chain, revokerId = SIDS.govRoot, effectiveEpoch = 20, scope } = {}) {
  return signRevocationRecord({
    effective_epoch: effectiveEpoch,
    revoker_id: revokerId,
    scope: scope ?? {
      actions: ['derive_adapter', 'resolve'],
      adapters: ['adapter:guarded-demo'],
      resources: ['resource:alpha'],
    },
    target_kind: 'chain',
    target_ref: chainDigest(chain),
    version: 'revocation/v1',
  });
}

export function buildRevocationContextSingle(overrides = {}) {
  const chain = buildValidSingle();
  return {
    capability_chain: chain,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
    revocation_records: [],
    trust_anchors: [SIDS.govRoot],
    ...overrides,
    capability_chain: overrides.capability_chain ?? chain,
  };
}

export function buildRevocationContextDelegated(overrides = {}) {
  const chain = buildValidDelegated();
  return {
    capability_chain: chain,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
    revocation_records: [],
    trust_anchors: [SIDS.govRoot],
    ...overrides,
    capability_chain: overrides.capability_chain ?? chain,
  };
}
