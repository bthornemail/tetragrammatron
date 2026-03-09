import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { EABIHost } from '../src/eabi/host.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { loadProtocolFixture } from '../tests/protocol/fixture.mjs';

export async function runEABIDemoRoutedCall() {
  const fixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'eabi-demo-route-'));
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

  const invocation = {
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
  };

  const result = await eabi.invoke(invocation);
  return {
    acceptance: 'eabi routed-call framing remains transport-neutral over hd-rpc semantics',
    invocation,
    result,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const out = await runEABIDemoRoutedCall();
  process.stdout.write(`${JSON.stringify(out, null, 2)}\n`);
}
