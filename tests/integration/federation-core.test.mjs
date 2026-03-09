import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { deriveSchemaDigest } from '../../src/protocol/idl.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { demoResolveCall } from '../../scripts/_shared.mjs';
import { makeAnnouncement, makeDescriptor } from '../federation/fixture.mjs';

test('federated call is semantically equivalent to direct core call', async () => {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'fed-core-'));
  const host = await CoreHost.create({ repoDir: repo });

  const hd = new HDRPC();
  hd.registerTarget('node-a', host);

  const schemaDigest = deriveSchemaDigest(demoResolveCall().canonical_input.schema);
  const announcement = makeAnnouncement({
    descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', priority: 1, schema_digests: [schemaDigest] }),
    provider_id: 'provider:a',
  });
  assert.equal(hd.announceProvider(announcement, { now_epoch: 20 }).ok, true);

  const seeded = await host.resolve(demoResolveCall());
  const sid = seeded.identity.sid;

  const direct = await host.resolve(demoResolveCall());
  const routed = await hd.call(sid, 'Normalized', {
    canonical_input: demoResolveCall().canonical_input,
  });

  const { federation, network, ...routedCoreShape } = routed;
  void federation;
  void network;
  assert.equal(canonicalJson(routedCoreShape), canonicalJson(direct));
});
