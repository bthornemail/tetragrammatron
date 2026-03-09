import { deriveSchemaDigest } from '../src/protocol/idl.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall } from './_shared.mjs';
import { FED, makeAnnouncement, makeDescriptor } from '../tests/federation/fixture.mjs';

function makeTopology(order, schemaDigest) {
  const map = {
    a: makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', priority: 1, schema_digests: [schemaDigest] }), provider_id: 'provider:a' }),
    b: makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:b', priority: 2, schema_digests: [schemaDigest] }), provider_id: 'provider:b' }),
    c: makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-c', provider_id: 'provider:c', priority: 1, schema_digests: [schemaDigest] }), provider_id: 'provider:c' }),
  };
  return order.map((k) => map[k]);
}

export async function runFederationDeterministicArbitrationDemo() {
  const schemaDigest = deriveSchemaDigest(demoResolveCall().canonical_input.schema);
  const request = {
    federation_scope: '',
    schema_digest: schemaDigest,
    sid: FED.sidA,
    stage: 'Normalized',
  };

  const hd1 = new HDRPC();
  for (const a of makeTopology(['a', 'b', 'c'], schemaDigest)) {
    hd1.announceProvider(a, { now_epoch: 20 });
  }

  const hd2 = new HDRPC();
  for (const a of makeTopology(['c', 'a', 'b'], schemaDigest)) {
    hd2.announceProvider(a, { now_epoch: 20 });
  }

  const r1 = hd1.deriveFederationRouteSet(request, { now_epoch: 20 });
  const r2 = hd2.deriveFederationRouteSet(request, { now_epoch: 20 });
  const a1 = hd1.arbitrateFederationRoute(r1.value);
  const a2 = hd2.arbitrateFederationRoute(r2.value);

  return {
    acceptance: 'same topology and inputs produce the same arbitration result independent of announcement order',
    same_selected_provider: a1.ok && a2.ok && a1.value.selected.provider_id === a2.value.selected.provider_id,
    selected_provider: a1.ok ? a1.value.selected.provider_id : null,
    tie_break_fields: a1.ok ? a1.value.arbitration_fields : [],
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runFederationDeterministicArbitrationDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
