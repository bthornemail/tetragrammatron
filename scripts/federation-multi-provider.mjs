import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { canonicalJson } from '../src/protocol/dbc.mjs';
import { deriveSchemaDigest } from '../src/protocol/idl.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall } from './_shared.mjs';
import { makeAnnouncement, makeDescriptor } from '../tests/federation/fixture.mjs';

export async function runFederationMultiProviderDemo() {
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'fed-demo-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'fed-demo-b-'));
  const repoC = await mkdtemp(path.join(os.tmpdir(), 'fed-demo-c-'));
  const hostA = await CoreHost.create({ repoDir: repoA });
  const hostB = await CoreHost.create({ repoDir: repoB });
  const hostC = await CoreHost.create({ repoDir: repoC });

  const hd = new HDRPC();
  hd.registerTarget('node-a', hostA);
  hd.registerTarget('node-b', hostB);
  hd.registerTarget('node-c', hostC);

  const schemaDigest = deriveSchemaDigest(demoResolveCall().canonical_input.schema);
  hd.announceProvider(makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', priority: 1, schema_digests: [schemaDigest] }), provider_id: 'provider:a' }), { now_epoch: 20 });
  hd.announceProvider(makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:b', priority: 3, schema_digests: [schemaDigest] }), provider_id: 'provider:b' }), { now_epoch: 20 });
  hd.announceProvider(makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-c', provider_id: 'provider:c', priority: 2, schema_digests: [schemaDigest] }), provider_id: 'provider:c' }), { now_epoch: 20 });

  const seeded = await hostA.resolve(demoResolveCall());
  const sid = seeded.identity.sid;

  const direct = await hostB.resolve(demoResolveCall());
  const routed = await hd.call(sid, 'Normalized', { canonical_input: demoResolveCall().canonical_input });
  const { federation, network, ...routedCore } = routed;

  return {
    acceptance: 'discover -> routeset -> arbitrate -> forward is deterministic over multi-provider topology',
    equivalence: canonicalJson(direct) === canonicalJson(routedCore),
    provider_count: hd.listFederationProviders().length,
    selected_provider: federation?.selected?.provider_id ?? null,
    selected_target: network?.route_target ?? null,
    sid,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runFederationMultiProviderDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
