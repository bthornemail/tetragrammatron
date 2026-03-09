import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { EABIHost } from '../../src/eabi/host.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('routed-call framing maps through HD-RPC deterministically', async () => {
  const fixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'eabi-net-'));
  const core = await CoreHost.create({ repoDir: repo });
  const hd = new HDRPC();
  hd.registerTarget('node-a', core);

  const seed = await core.resolve({
    canonical_input: {
      document: fixture.golden.canonical_success.document,
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  });
  hd.registerRoute(seed.identity.sid, 'node-a');

  const eabi = new EABIHost({ coreHost: core, hdRpc: hd });

  const out = await eabi.invoke({
    eabi_version: '1.0',
    operation: 'routed-call',
    context: {},
    payload: {
      canonical_input: {
        document: fixture.golden.canonical_success.document,
        schema: fixture.schema,
        view: { target: 'projection/json-v1' },
      },
      sid: seed.identity.sid,
      target_stage: 'Normalized',
    },
  });

  assert.equal(out.ok, true);
  assert.equal(out.operation, 'routed-call');
  assert.equal(out.result.abi_version, 'ABI-1.1.0');
});
