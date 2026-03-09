import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { EABIHost } from '../../src/eabi/host.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

async function setup() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'eabi-host-'));
  const core = await CoreHost.create({ repoDir: repo });
  const hd = new HDRPC();
  hd.registerTarget('node-a', core);
  return { core, eabi: new EABIHost({ coreHost: core, hdRpc: hd }), hd };
}

test('resolve invocation returns success envelope with semantic result or reject', async () => {
  const { eabi } = await setup();
  const fixture = await loadProtocolFixture();

  const success = await eabi.invoke({
    eabi_version: '1.0',
    operation: 'resolve',
    context: {},
    payload: {
      canonical_input: { d: fixture.golden.canonical_success.document },
      schema: fixture.schema,
      schema_digest: 'sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8',
      target_stage: 'Normalized',
    },
  });
  assert.equal(success.ok, true);
  assert.equal(success.operation, 'resolve');
  assert.equal(success.result.abi_version, 'ABI-1.1.0');

  const reject = await eabi.invoke({
    eabi_version: '1.0',
    operation: 'resolve',
    context: {},
    payload: {
      canonical_input: { d: fixture.negative.normalize_reject.document },
      schema: fixture.schema,
      schema_digest: 'sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8',
      target_stage: 'Normalized',
    },
  });
  assert.equal(reject.ok, true);
  assert.equal(reject.result.reject_kind, 'RejectNormalize');
});

test('execution errors remain EABI-level errors', async () => {
  const { eabi } = await setup();
  const out = await eabi.invoke({
    eabi_version: '9.9',
    operation: 'resolve',
    context: {},
    payload: {},
  });
  assert.equal(out.ok, false);
  assert.equal(out.error.code, 'unsupported_version');
});
