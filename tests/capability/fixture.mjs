import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { signGrant } from '../../src/protocol/capability.mjs';

export const SIDS = {
  actorA: 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  actorB: 'sid:dbc:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
  actorC: 'sid:dbc:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc',
  govRoot: 'sid:dbc:1111111111111111111111111111111111111111111111111111111111111111',
  govX: 'sid:dbc:2222222222222222222222222222222222222222222222222222222222222222',
  govY: 'sid:dbc:3333333333333333333333333333333333333333333333333333333333333333',
  subject: 'sid:dbc:9999999999999999999999999999999999999999999999999999999999999999',
};

export async function loadCaseIds(kind) {
  const p = path.join(process.cwd(), 'fixtures', 'capability', kind, 'cases.json');
  const raw = await readFile(p, 'utf8');
  return JSON.parse(raw).cases;
}

export function baseScope() {
  return {
    actions: ['derive_adapter', 'resolve'],
    adapters: ['adapter:guarded-demo'],
    resources: ['resource:alpha'],
  };
}

export function makeGrant({ governor_sid, actor_sid, subject_sid = SIDS.subject, parent_grant_id = null, not_before = 10, not_after = 50, scope = baseScope() }) {
  return {
    actor_sid,
    epoch: { not_after, not_before },
    governor_sid,
    parent_grant_id,
    scope,
    subject_sid,
    version: 'capability/v1',
  };
}

export function buildValidSingle() {
  return [signGrant(makeGrant({ governor_sid: SIDS.govRoot, actor_sid: SIDS.actorA }))];
}

export function buildValidDelegated() {
  const g1 = signGrant(makeGrant({ governor_sid: SIDS.govRoot, actor_sid: SIDS.govX }));
  const g2 = signGrant(makeGrant({ governor_sid: SIDS.govX, actor_sid: SIDS.actorA, parent_grant_id: g1.grant_id }));
  return [g1, g2];
}

export function buildValidMultiIntermediate() {
  const g1 = signGrant(makeGrant({ governor_sid: SIDS.govRoot, actor_sid: SIDS.govX }));
  const g2 = signGrant(makeGrant({ governor_sid: SIDS.govX, actor_sid: SIDS.govY, parent_grant_id: g1.grant_id }));
  const g3 = signGrant(makeGrant({ governor_sid: SIDS.govY, actor_sid: SIDS.actorA, parent_grant_id: g2.grant_id }));
  return [g1, g2, g3];
}
