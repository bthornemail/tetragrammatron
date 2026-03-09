import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';
import { signRevocationRecord } from '../../src/revocation/schema.mjs';

function normalizedCall(fixture) {
  return {
    canonical_input: {
      derivation_context: {},
      document: fixture.golden.canonical_success.document,
      federation_scope: '',
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  };
}

test('in-process resolve yields stage envelope, persisted refs, SID and descriptor', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const result = await host.resolve(normalizedCall(fixture));

  assert.equal(result.ok, true);
  assert.equal(result.envelope.stage, 'Normalized');
  assert.equal(result.meta.category, 'success');
  assert.equal(result.value_kind, 'NormalForm');
  assert.equal(typeof result.persisted.call_ref, 'string');
  assert.equal(typeof result.persisted.result_ref, 'string');
  assert.equal(typeof result.persisted.normal_form_ref, 'string');
  assert.equal(typeof result.persisted.descriptor_ref, 'string');
  assert.equal(typeof result.identity.sid, 'string');
  assert.equal(result.identity.descriptor.sid, result.identity.sid);
  const events = host.listEvents();
  assert.equal(events.some((e) => e.kind === 'resolution.started'), true);
  assert.equal(events.some((e) => e.kind === 'resolution.succeeded'), true);
});

test('resolve distinguishes host validation failure from protocol reject', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const malformed = await host.resolve({ target_stage: 'Normalized' });
  assert.equal(malformed.ok, false);
  assert.equal(malformed.code, 'invalid_request');
  assert.equal(malformed.meta.category, 'host_validation_failure');

  const protocolReject = await host.resolve({
    canonical_input: {
      document: fixture.negative.normalize_reject.document,
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  });
  assert.equal(protocolReject.ok, false);
  assert.equal(protocolReject.reject_kind, 'RejectNormalize');
  assert.equal(protocolReject.meta.category, 'protocol_reject');
  const events = host.listEvents();
  assert.equal(events.filter((e) => e.kind === 'resolution.rejected').length >= 2, true);
});

test('descriptor lookup by SID returns exact descriptor', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const result = await host.resolve(normalizedCall(fixture));
  const lookup = await host.getDescriptorBySID(result.identity.sid);

  assert.equal(lookup.ok, true);
  assert.deepEqual(lookup.descriptor, result.identity.descriptor);
  assert.equal(host.listEvents().some((e) => e.kind === 'descriptor.lookup_succeeded'), true);
});

test('capability verification returns deterministic typed result', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const result = await host.verifyCapability({});
  assert.equal(result.ok, false);
  assert.equal(result.status, 'invalid_request');
  assert.equal(result.kind, 'CapabilityRejected');
  assert.equal(typeof result.meta.evidence_ref, 'string');
  assert.equal(host.listEvents().some((e) => e.kind === 'capability.verify_failed'), true);
});

test('revocation verification returns deterministic typed result', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });
  const chain = buildValidSingle();
  const record = signRevocationRecord({
    effective_epoch: 20,
    revoker_id: SIDS.govRoot,
    scope: {
      actions: ['resolve'],
      adapters: ['adapter:guarded-demo'],
      resources: ['resource:alpha'],
    },
    target_kind: 'grant',
    target_ref: chain[0].grant_id,
    version: 'revocation/v1',
  });

  const result = await host.verifyRevocation({
    capability_chain: chain,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
    revocation_records: [record],
    trust_anchors: [SIDS.govRoot],
  });
  assert.equal(result.ok, false);
  assert.equal(result.status, 'revoked');
  assert.equal(result.kind, 'RevocationApplied');
  assert.equal(typeof result.meta.evidence_ref, 'string');
  const events = host.listEvents();
  assert.equal(events.some((e) => e.kind === 'capability.revocation_recorded'), true);
  assert.equal(events.some((e) => e.kind === 'capability.revocation_applied'), true);
});

test('resolve capability gating and guarded adapter behavior are deterministic', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const denied = await host.resolve({
    ...normalizedCall(fixture),
    required_capability: true,
    capability_context: {},
  });
  assert.equal(denied.ok, false);
  assert.equal(denied.code, 'capability_denied');
  assert.equal(denied.meta.category, 'host_validation_failure');

  const validCapability = {
    capability_chain: buildValidSingle(),
    max_delegation_depth: 8,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
    trust_anchors: [SIDS.govRoot],
  };

  const allowed = await host.resolve({
    ...normalizedCall(fixture),
    required_capability: true,
    capability_context: validCapability,
  });
  assert.equal(allowed.ok, true);

  const guardedDenied = await host.deriveAdapter('adapter:guarded-demo', SIDS.subject, { capability_context: {} });
  assert.equal(guardedDenied.ok, false);
  assert.equal(guardedDenied.code, 'adapter_not_authorized');

  const guardedAllowed = await host.deriveAdapter('adapter:guarded-demo', SIDS.subject, {
    capability_context: {
      ...validCapability,
      request: {
        action: 'derive_adapter',
        actor_sid: SIDS.actorA,
        adapter_label: 'adapter:guarded-demo',
        resource: 'resource:alpha',
        subject_sid: SIDS.subject,
      },
    },
  });
  assert.equal(guardedAllowed.ok, true);
  assert.equal(guardedAllowed.value.adapter_label, 'adapter:guarded-demo');
  const events = host.listEvents();
  assert.equal(events.some((e) => e.kind === 'adapter.derived'), true);
  assert.equal(events.some((e) => e.kind === 'adapter.derivation_failed'), true);

  const revokedRecord = signRevocationRecord({
    effective_epoch: 20,
    revoker_id: SIDS.govRoot,
    scope: {
      actions: ['resolve'],
      adapters: ['adapter:guarded-demo'],
      resources: ['resource:alpha'],
    },
    target_kind: 'grant',
    target_ref: validCapability.capability_chain[0].grant_id,
    version: 'revocation/v1',
  });
  const revokedDenied = await host.resolve({
    ...normalizedCall(fixture),
    required_capability: true,
    capability_context: {
      ...validCapability,
      revocation_records: [revokedRecord],
    },
  });
  assert.equal(revokedDenied.ok, false);
  assert.equal(revokedDenied.code, 'capability_denied');
});

test('missing SID lookup and unsupported adapter are deterministic non-success', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const missing = await host.getDescriptorBySID('sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa');
  assert.deepEqual(missing, {
    code: 'sid_not_found',
    kind: 'CoreLookupFailure',
    ok: false,
    sid: 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  });

  const unsupported = await host.deriveAdapter('adapter:bogus', 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa');
  assert.equal(unsupported.ok, false);
  assert.equal(unsupported.code, 'unsupported_adapter');
  assert.equal(unsupported.kind, 'RejectAdapter');
});
