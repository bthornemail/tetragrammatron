import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { deterministicEventView } from '../src/evr/schema.mjs';
import { CoreHost } from '../src/core/host.mjs';
import { HubShell } from '../src/hub/shell.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { CAP_SIDS, buildExpiredCapabilityContext, buildValidCapabilityContext } from './capability-common.mjs';

function compact(events) {
  return events.map((event) => deterministicEventView(event));
}

export async function runEVRCapabilityTrace() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'evr-cap-'));
  const coreHost = await CoreHost.create({ repoDir: repo });
  const hdRpc = new HDRPC();
  hdRpc.registerTarget('node-a', coreHost);
  const shell = HubShell.create({ coreHost, hdRpc });

  const valid = buildValidCapabilityContext();
  const expired = buildExpiredCapabilityContext();

  await shell.run('capability.verify', { input: valid });
  await shell.run('capability.verify', { input: expired });
  await shell.run('routing.adapter', {
    label: 'adapter:guarded-demo',
    sid: CAP_SIDS.subject,
  });

  const timeline = await shell.run('events.timeline', {
    filter: { family: 'capability' },
    limit: 200,
  });

  return {
    acceptance: 'EVR capability trace preserves deterministic success/failure evidence shapes',
    events: compact(timeline.value),
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runEVRCapabilityTrace();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
