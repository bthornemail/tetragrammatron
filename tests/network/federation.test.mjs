import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { deriveSchemaDigest } from '../../src/protocol/idl.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { demoResolveCall } from '../../scripts/_shared.mjs';
import { makeAnnouncement, makeDescriptor, makeReplayRecord } from '../federation/fixture.mjs';

test('federation discover/arbitrate/call pipeline is deterministic and typed', async () => {
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'fed-net-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'fed-net-b-'));
  const hostA = await CoreHost.create({ repoDir: repoA });
  const hostB = await CoreHost.create({ repoDir: repoB });

  const hd = new HDRPC();
  hd.registerTarget('node-a', hostA);
  hd.registerTarget('node-b', hostB);

  const schemaDigest = deriveSchemaDigest(demoResolveCall().canonical_input.schema);
  const annA = makeAnnouncement({
    descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', priority: 1, schema_digests: [schemaDigest] }),
    provider_id: 'provider:a',
  });
  const annB = makeAnnouncement({
    descriptor: makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:b', priority: 2, schema_digests: [schemaDigest] }),
    provider_id: 'provider:b',
  });

  assert.equal(hd.announceProvider(annA, { now_epoch: 20 }).ok, true);
  assert.equal(hd.announceProvider(annB, { now_epoch: 20 }).ok, true);

  const seed = await hostA.resolve(demoResolveCall());
  const sid = seed.identity.sid;

  const routeSet = hd.deriveFederationRouteSet({
    federation_scope: '',
    schema_digest: schemaDigest,
    sid,
    stage: 'Normalized',
  }, { now_epoch: 20 });
  assert.equal(routeSet.ok, true);
  assert.equal(routeSet.value.candidates.length, 2);

  const arbitration = hd.arbitrateFederationRoute(routeSet.value);
  assert.equal(arbitration.ok, true);
  assert.equal(arbitration.value.selected.provider_id, 'provider:b');

  const routed = await hd.call(sid, 'Normalized', {
    canonical_input: demoResolveCall().canonical_input,
  });
  assert.equal(routed.ok, true);
  assert.equal(routed.federation.selected.provider_id, 'provider:b');

  const events = hd.listEvents();
  assert.equal(events.some((e) => e.kind === 'federation.announcement_received'), true);
  assert.equal(events.some((e) => e.kind === 'federation.routeset_derived'), true);
  assert.equal(events.some((e) => e.kind === 'federation.arbitration_selected'), true);
});

test('federation conflict and convergence/divergence outcomes are typed', async () => {
  const hd = new HDRPC();
  const schemaDigest = deriveSchemaDigest(demoResolveCall().canonical_input.schema);

  const dA = makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', schema_digests: [schemaDigest] });
  const dB = makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:a', schema_digests: [schemaDigest] });

  assert.equal(hd.announceProvider(makeAnnouncement({ descriptor: dA, provider_id: 'provider:a' }), { now_epoch: 20 }).ok, true);
  const conflict = hd.announceProvider(makeAnnouncement({ descriptor: dB, provider_id: 'provider:a' }), { now_epoch: 20 });
  assert.equal(conflict.ok, false);
  assert.equal(conflict.code, 'announcement_conflict');

  const converged = hd.checkFederationConvergence(makeReplayRecord(), makeReplayRecord());
  assert.equal(converged.ok, true);
  const diverged = hd.checkFederationConvergence(
    makeReplayRecord({ replay_digest: 'sha256:1111111111111111111111111111111111111111111111111111111111111111' }),
    makeReplayRecord({ replay_digest: 'sha256:2222222222222222222222222222222222222222222222222222222222222222' }),
  );
  assert.equal(diverged.ok, false);
  assert.equal(diverged.code, 'replay_divergence');

  const events = hd.listEvents();
  assert.equal(events.some((e) => e.kind === 'federation.convergence_witness'), true);
  assert.equal(events.some((e) => e.kind === 'federation.divergence_witness'), true);
});
