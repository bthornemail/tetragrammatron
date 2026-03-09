import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { canonicalJson } from '../src/protocol/dbc.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall, unknownSid } from './_shared.mjs';

export async function runMinimalFederationProof() {
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'fed-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'fed-b-'));

  const hostA = await CoreHost.create({ repoDir: repoA });
  const hostB = await CoreHost.create({ repoDir: repoB });

  const network = new HDRPC();
  network.registerTarget('node-a', hostA);
  network.registerTarget('node-b', hostB);

  const resolved = await hostA.resolve(demoResolveCall());
  network.registerRoute(resolved.identity.sid, 'node-a');

  const direct = await hostA.resolve(demoResolveCall());
  const routed = await network.call(resolved.identity.sid, 'Normalized', {
    canonical_input: demoResolveCall().canonical_input,
  });

  const { network: routingMeta, ...routedCore } = routed;
  const unknown = await network.call(unknownSid(), 'Normalized', {
    canonical_input: demoResolveCall().canonical_input,
  });

  return {
    acceptance: 'HD-RPC is semantic routing over Core, not semantic reinterpretation',
    equivalence: canonicalJson(direct) === canonicalJson(routedCore),
    route_target: routingMeta?.route_target ?? null,
    sid: resolved.identity.sid,
    unknown_route_result: unknown,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runMinimalFederationProof();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
