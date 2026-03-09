import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { NRR } from '../../src/substrate/nrr.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { demoResolveCall } from '../../scripts/_shared.mjs';
import { runNodePipelineDemo } from '../../scripts/node-pipeline-demo.mjs';

test('node pipeline demo is canonically stable and projection-only outside resolve', async () => {
  const result = await runNodePipelineDemo();

  assert.equal(result.resolve.value.ok, true);
  assert.equal(result.resolve.value.stage, 'Normalized');
  assert.equal(result.descriptor.value.ok, true);
  assert.equal(result.adapter.value.ok, true);
  assert.equal(result.store.value.summary.nrr_verify_ok, true);
  assert.equal(result.store.value.summary.replay_readiness_ok, true);
});

test('export/import preserves descriptor and adapter lookup behavior', async () => {
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'pipeline-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'pipeline-b-'));

  const hostA = await CoreHost.create({ repoDir: repoA });
  const resolvedA = await hostA.resolve(demoResolveCall());
  const sid = resolvedA.identity.sid;

  const bundle = await hostA.nrr.exportBundle();
  const nrrB = new NRR(repoB);
  await nrrB.init();
  await nrrB.importBundle(bundle);

  const hostB = new CoreHost({ nrr: nrrB });
  const descriptorB = await hostB.getDescriptorBySID(sid);
  assert.equal(descriptorB.ok, true);
  assert.deepEqual(descriptorB.descriptor, resolvedA.identity.descriptor);

  const netA = new HDRPC();
  const netB = new HDRPC();
  const ipv6A = netA.deriveAdapter('adapter:ipv6', sid, { scope: 'pipeline' });
  const ipv6B = netB.deriveAdapter('adapter:ipv6', sid, { scope: 'pipeline' });
  assert.equal(ipv6A.ok, true);
  assert.equal(ipv6B.ok, true);
  assert.equal(ipv6A.value.credential, ipv6B.value.credential);
});
