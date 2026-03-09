import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { HubShell } from '../src/hub/shell.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall } from './_shared.mjs';

export async function runNodePipelineDemo() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'pipeline-'));
  const coreHost = await CoreHost.create({ repoDir: repo });
  const hdRpc = new HDRPC();
  hdRpc.registerTarget('node-a', coreHost);

  const shell = HubShell.create({ coreHost, hdRpc });

  const resolved = await shell.run('resolve', { call: demoResolveCall() });
  const sid = resolved.value.identity?.sid;
  hdRpc.registerRoute(sid, 'node-a');

  const descriptor = await shell.run('descriptor.lookup', { sid });
  const adapter = await shell.run('routing.adapter', { label: 'adapter:ipv6', sid });
  const store = await shell.run('store.inspect');
  const events = await shell.run('events.list');

  return {
    acceptance: 'resolve is the only canonical write path; all other views are projection',
    adapter,
    descriptor,
    events,
    resolve: resolved,
    store,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runNodePipelineDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
