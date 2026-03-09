import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { deterministicEventView } from '../src/evr/schema.mjs';
import { CoreHost } from '../src/core/host.mjs';
import { HubShell } from '../src/hub/shell.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall } from './_shared.mjs';

function compact(events) {
  return events.map((event) => deterministicEventView(event));
}

export async function runEVRResolveTrace() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'evr-resolve-'));
  const coreHost = await CoreHost.create({ repoDir: repo });
  const hdRpc = new HDRPC();
  hdRpc.registerTarget('node-a', coreHost);
  const shell = HubShell.create({ coreHost, hdRpc });

  await shell.run('resolve', { call: demoResolveCall() });
  const timeline = await shell.run('events.timeline', {
    filter: { family: 'resolution' },
    limit: 200,
  });

  return {
    acceptance: 'EVR resolve trace is observational evidence over canonical resolve behavior',
    events: compact(timeline.value),
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runEVRResolveTrace();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
