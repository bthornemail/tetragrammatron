import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { deriveSchemaDigest } from '../../src/protocol/idl.mjs';
import { HubShell } from '../../src/hub/shell.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { demoResolveCall } from '../../scripts/_shared.mjs';
import { makeAnnouncement, makeDescriptor, makeReplayRecord } from '../federation/fixture.mjs';

async function setup() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'hub-fed-'));
  const core = await CoreHost.create({ repoDir: repo });
  const hd = new HDRPC();
  hd.registerTarget('node-a', core);

  const schemaDigest = deriveSchemaDigest(demoResolveCall().canonical_input.schema);
  hd.announceProvider(makeAnnouncement({
    descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', schema_digests: [schemaDigest] }),
    provider_id: 'provider:a',
  }), { now_epoch: 20 });

  const seed = await core.resolve(demoResolveCall());
  const shell = HubShell.create({ coreHost: core, hdRpc: hd });
  return { core, schemaDigest, shell, sid: seed.identity.sid };
}

test('hub federation panes are projection-only over network federation state', async () => {
  const { core, schemaDigest, shell, sid } = await setup();
  const baseline = (await core.nrr.log()).length;

  const providers = await shell.run('federation.providers');
  assert.equal(providers.pane, 'federation.providers');
  assert.equal(Array.isArray(providers.value), true);
  assert.equal(providers.value.length, 1);

  const routeSet = await shell.run('federation.routeset', {
    options: { now_epoch: 20 },
    request: { federation_scope: '', schema_digest: schemaDigest, sid, stage: 'Normalized' },
  });
  assert.equal(routeSet.value.ok, true);

  const arb = await shell.run('federation.arbitration', { route_set: routeSet.value.value });
  assert.equal(arb.value.ok, true);

  const conv = await shell.run('federation.convergence', {
    local_record: makeReplayRecord(),
    remote_record: makeReplayRecord(),
  });
  assert.equal(conv.value.ok, true);

  const after = (await core.nrr.log()).length;
  assert.equal(after, baseline);
});
