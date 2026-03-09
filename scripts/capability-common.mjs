import { signGrant } from '../src/protocol/capability.mjs';
import { signRevocationRecord } from '../src/revocation/schema.mjs';

export const CAP_SIDS = {
  actorA: 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  actorB: 'sid:dbc:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
  govRoot: 'sid:dbc:1111111111111111111111111111111111111111111111111111111111111111',
  govX: 'sid:dbc:2222222222222222222222222222222222222222222222222222222222222222',
  subject: 'sid:dbc:9999999999999999999999999999999999999999999999999999999999999999',
};

export function baseScope() {
  return {
    actions: ['derive_adapter', 'resolve'],
    adapters: ['adapter:guarded-demo'],
    resources: ['resource:alpha'],
  };
}

function makeGrant({
  governor_sid,
  actor_sid,
  parent_grant_id = null,
  subject_sid = CAP_SIDS.subject,
  not_before = 10,
  not_after = 50,
  scope = baseScope(),
}) {
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

export function buildValidCapabilityContext() {
  return {
    capability_chain: [
      signGrant(makeGrant({
        actor_sid: CAP_SIDS.actorA,
        governor_sid: CAP_SIDS.govRoot,
      })),
    ],
    max_delegation_depth: 8,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: CAP_SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: CAP_SIDS.subject,
    },
    trust_anchors: [CAP_SIDS.govRoot],
  };
}

export function buildExpiredCapabilityContext() {
  const base = buildValidCapabilityContext();
  return {
    ...base,
    now_epoch: 99,
  };
}

export function buildScopeEscalationContext() {
  const root = signGrant(makeGrant({
    actor_sid: CAP_SIDS.govX,
    governor_sid: CAP_SIDS.govRoot,
  }));
  const escalated = signGrant(makeGrant({
    actor_sid: CAP_SIDS.actorA,
    governor_sid: CAP_SIDS.govX,
    parent_grant_id: root.grant_id,
    scope: {
      actions: ['delete', 'derive_adapter', 'resolve'],
      adapters: ['adapter:guarded-demo'],
      resources: ['resource:alpha'],
    },
  }));

  return {
    capability_chain: [root, escalated],
    max_delegation_depth: 8,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: CAP_SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: CAP_SIDS.subject,
    },
    trust_anchors: [CAP_SIDS.govRoot],
  };
}

export function buildRevokedCapabilityContext() {
  const base = buildValidCapabilityContext();
  return {
    ...base,
    revocation_records: [signRevocationRecord({
      effective_epoch: 20,
      revoker_id: CAP_SIDS.govRoot,
      scope: {
        actions: ['resolve'],
        adapters: ['adapter:guarded-demo'],
        resources: ['resource:alpha'],
      },
      target_kind: 'grant',
      target_ref: base.capability_chain[0].grant_id,
      version: 'revocation/v1',
    })],
  };
}

export function buildDelegatedRevokedAncestorContext() {
  const root = signGrant(makeGrant({
    actor_sid: CAP_SIDS.govX,
    governor_sid: CAP_SIDS.govRoot,
  }));
  const leaf = signGrant(makeGrant({
    actor_sid: CAP_SIDS.actorA,
    governor_sid: CAP_SIDS.govX,
    parent_grant_id: root.grant_id,
  }));
  return {
    capability_chain: [root, leaf],
    max_delegation_depth: 8,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: CAP_SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: CAP_SIDS.subject,
    },
    revocation_records: [signRevocationRecord({
      effective_epoch: 20,
      revoker_id: CAP_SIDS.govRoot,
      scope: {
        actions: ['resolve'],
        adapters: ['adapter:guarded-demo'],
        resources: ['resource:alpha'],
      },
      target_kind: 'grant',
      target_ref: root.grant_id,
      version: 'revocation/v1',
    })],
    trust_anchors: [CAP_SIDS.govRoot],
  };
}

export function buildUnauthorizedRevocationContext() {
  const base = buildValidCapabilityContext();
  return {
    ...base,
    revocation_records: [signRevocationRecord({
      effective_epoch: 20,
      revoker_id: CAP_SIDS.actorB,
      scope: {
        actions: ['resolve'],
        adapters: ['adapter:guarded-demo'],
        resources: ['resource:alpha'],
      },
      target_kind: 'grant',
      target_ref: base.capability_chain[0].grant_id,
      version: 'revocation/v1',
    })],
  };
}
