import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { deterministicEventView } from '../src/evr/schema.mjs';
import { CoreHost } from '../src/core/host.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall, unknownSid } from './_shared.mjs';

function compact(events) {
  return events.map((event) => deterministicEventView(event));
}

export async function runEVRRouteTrace() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'evr-route-'));
  const coreHost = await CoreHost.create({ repoDir: repo });
  const hdRpc = new HDRPC();
  hdRpc.registerTarget('node-a', coreHost);

  const resolved = await coreHost.resolve(demoResolveCall());
  hdRpc.registerRoute(resolved.identity.sid, 'node-a');

  await hdRpc.call(resolved.identity.sid, 'Normalized', { canonical_input: demoResolveCall().canonical_input });
  await hdRpc.call(unknownSid(), 'Normalized', { canonical_input: demoResolveCall().canonical_input });

  return {
    acceptance: 'EVR route trace distinguishes lookup success/failure and call forwarding/failure',
    events: compact(hdRpc.listEvents(200)),
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runEVRRouteTrace();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
