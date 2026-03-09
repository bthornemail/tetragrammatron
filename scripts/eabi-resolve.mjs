import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { EABIHost } from '../src/eabi/host.mjs';
import { loadProtocolFixture } from '../tests/protocol/fixture.mjs';

export async function runEABIDemoResolve() {
  const fixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'eabi-demo-resolve-'));
  const core = await CoreHost.create({ repoDir: repo });
  const eabi = new EABIHost({ coreHost: core, hdRpc: null });

  const invocation = {
    eabi_version: '1.0',
    operation: 'resolve',
    context: {},
    payload: {
      canonical_input: { d: fixture.golden.canonical_success.document },
      schema: fixture.schema,
      schema_digest: 'sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8',
      target_stage: 'Normalized',
    },
  };

  const result = await eabi.invoke(invocation);
  return {
    acceptance: 'eabi resolve framing preserves semantic result without mutation',
    invocation,
    result,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const out = await runEABIDemoResolve();
  process.stdout.write(`${JSON.stringify(out, null, 2)}\n`);
}
