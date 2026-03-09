import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { EABIHost } from '../src/eabi/host.mjs';
import { SIDS, buildValidSingle } from '../tests/capability/fixture.mjs';

export async function runEABIDemoCapabilityVerify() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'eabi-demo-cap-'));
  const core = await CoreHost.create({ repoDir: repo });
  const eabi = new EABIHost({ coreHost: core, hdRpc: null });

  const invocation = {
    eabi_version: '1.0',
    operation: 'verify-capability',
    context: {},
    payload: {
      capability_input: {
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
    },
  };

  const result = await eabi.invoke(invocation);
  return {
    acceptance: 'eabi capability verify framing preserves typed semantic outcome',
    invocation,
    result,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const out = await runEABIDemoCapabilityVerify();
  process.stdout.write(`${JSON.stringify(out, null, 2)}\n`);
}
