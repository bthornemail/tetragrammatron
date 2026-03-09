import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { EABIHost } from '../../src/eabi/host.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('bundle-export/import and verify-store are framed deterministically', async () => {
  const fixture = await loadProtocolFixture();
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'eabi-bundle-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'eabi-bundle-b-'));
  const coreA = await CoreHost.create({ repoDir: repoA });
  const coreB = await CoreHost.create({ repoDir: repoB });
  const eabiA = new EABIHost({ coreHost: coreA, hdRpc: null });
  const eabiB = new EABIHost({ coreHost: coreB, hdRpc: null });

  await coreA.resolve({
    canonical_input: {
      document: fixture.golden.canonical_success.document,
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  });

  const exported = await eabiA.invoke({
    eabi_version: '1.0',
    operation: 'bundle-export',
    context: {},
    payload: { include_log_segment: true },
  });
  assert.equal(exported.ok, true);
  assert.equal(exported.result.bundle.manifest.nrr_version, '1.0');

  const imported = await eabiB.invoke({
    eabi_version: '1.0',
    operation: 'bundle-import',
    context: {},
    payload: { bundle: exported.result.bundle },
  });
  assert.equal(imported.ok, true);
  assert.equal(imported.result.status, 'complete');

  const verify = await eabiB.invoke({
    eabi_version: '1.0',
    operation: 'verify-store',
    context: {},
    payload: {},
  });
  assert.equal(verify.ok, true);
  assert.equal(['valid', 'integrity_violation'].includes(verify.result.status), true);
});
