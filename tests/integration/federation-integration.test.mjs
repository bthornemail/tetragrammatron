import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { deriveSchemaDigest } from '../../src/protocol/idl.mjs';
import { HubShell } from '../../src/hub/shell.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { demoResolveCall } from '../../scripts/_shared.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';
import { makeAnnouncement, makeDescriptor, makeReplayRecord } from '../federation/fixture.mjs';

async function loadIntegrationCases() {
  const p = path.join(process.cwd(), 'fixtures', 'federation', 'integration', 'cases.json');
  return JSON.parse(await readFile(p, 'utf8')).cases;
}

test('federation integration fixtures are present and full pipeline is deterministic', async () => {
  const cases = await loadIntegrationCases();
  assert.equal(cases.length >= 4, true);

  const repoA = await mkdtemp(path.join(os.tmpdir(), 'fed-int-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'fed-int-b-'));
  const hostA = await CoreHost.create({ repoDir: repoA });
  const hostB = await CoreHost.create({ repoDir: repoB });

  const hd = new HDRPC();
  hd.registerTarget('node-a', hostA);
  hd.registerTarget('node-b', hostB);

  const schemaDigest = deriveSchemaDigest(demoResolveCall().canonical_input.schema);
  hd.announceProvider(makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', priority: 1, schema_digests: [schemaDigest] }), provider_id: 'provider:a' }), { now_epoch: 20 });
  hd.announceProvider(makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:b', priority: 2, schema_digests: [schemaDigest] }), provider_id: 'provider:b' }), { now_epoch: 20 });

  const seed = await hostA.resolve(demoResolveCall());
  const sid = seed.identity.sid;

  const routed = await hd.call(sid, 'Normalized', {
    canonical_input: demoResolveCall().canonical_input,
    capability_context: {
      capability_chain: buildValidSingle(),
      now_epoch: 20,
      trust_anchors: [SIDS.govRoot],
      request: {
        action: 'resolve',
        actor_sid: SIDS.actorA,
        resource: 'resource:alpha',
        subject_sid: SIDS.subject,
      },
    },
    required_capability: true,
  });
  assert.equal(routed.ok, true);
  assert.equal(routed.federation.selected.provider_id, 'provider:b');

  const conv = hd.checkFederationConvergence(makeReplayRecord(), makeReplayRecord());
  assert.equal(conv.ok, true);

  const shell = HubShell.create({ coreHost: hostA, hdRpc: hd });
  const providers = await shell.run('federation.providers');
  assert.equal(Array.isArray(providers.value), true);
  assert.equal(providers.value.length, 2);

  const timeline = await shell.run('events.timeline', { filter: { family: 'federation' }, limit: 500 });
  assert.equal(timeline.value.length > 0, true);
  assert.equal(timeline.value.every((e) => e.family === 'federation'), true);

  const routedAgain = await hd.call(sid, 'Normalized', { canonical_input: demoResolveCall().canonical_input });
  const { network: _n1, ...coreShapeA } = routed;
  const { network: _n2, ...coreShapeB } = routedAgain;
  assert.equal(coreShapeA.ok, true);
  assert.equal(coreShapeB.ok, true);
  assert.equal(coreShapeA.stage, coreShapeB.stage);
  assert.equal(coreShapeA.value_kind, coreShapeB.value_kind);
  assert.equal(coreShapeA.identity.sid, coreShapeB.identity.sid);
  assert.equal(canonicalJson(coreShapeA.value), canonicalJson(coreShapeB.value));
});
