import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { EABIHost } from '../src/eabi/host.mjs';
import { loadProtocolFixture } from '../tests/protocol/fixture.mjs';

export async function runEABIDemoBundleRoundtrip() {
  const fixture = await loadProtocolFixture();
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'eabi-demo-bundle-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'eabi-demo-bundle-b-'));
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

  const imported = await eabiB.invoke({
    eabi_version: '1.0',
    operation: 'bundle-import',
    context: {},
    payload: { bundle: exported.result.bundle },
  });

  const verified = await eabiB.invoke({
    eabi_version: '1.0',
    operation: 'verify-store',
    context: {},
    payload: {},
  });

  return {
    acceptance: 'eabi bundle framing preserves replayable logical bundle contents',
    exported,
    imported,
    verified,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const out = await runEABIDemoBundleRoundtrip();
  process.stdout.write(`${JSON.stringify(out, null, 2)}\n`);
}
