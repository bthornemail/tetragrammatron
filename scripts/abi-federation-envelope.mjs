import { arbitrateRoute } from '../src/federation/arbitrate.mjs';
import { deriveRouteSet } from '../src/federation/routeset.mjs';
import {
  toArbitrationResultAbi,
  toConvergenceWitnessAbi,
  toFederationAnnouncementAbi,
  toRouteSetAbi,
} from '../src/abi/schema.mjs';
import { makeAnnouncement, makeDescriptor } from '../tests/federation/fixture.mjs';

export async function runABIDemoFederationEnvelope() {
  const descriptor = makeDescriptor({
    endpoint: 'node-a',
    provider_id: 'provider:a',
    schema_digests: ['sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8'],
  });
  const announcement = makeAnnouncement({ descriptor, provider_id: 'provider:a' });

  const routeSet = deriveRouteSet([announcement], {
    federation_scope: '',
    schema_digest: descriptor.schema_digests[0],
    sid: 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    stage: 'Normalized',
  }, { now_epoch: 20 });
  const arbitration = arbitrateRoute(routeSet.value);

  return {
    acceptance: 'abi federation envelopes preserve deterministic routeset and arbitration structures',
    announcement: toFederationAnnouncementAbi(announcement),
    arbitration: toArbitrationResultAbi(arbitration.value),
    convergence_witness: toConvergenceWitnessAbi({
      call_digest: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      result_digest: 'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
      runtime_sids: [
        'sid:dbc:1111111111111111111111111111111111111111111111111111111111111111',
        'sid:dbc:2222222222222222222222222222222222222222222222222222222222222222',
      ],
      stage: 'Normalized',
      test_id: 'abi-fed',
    }),
    route_set: toRouteSetAbi(routeSet.value),
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const out = await runABIDemoFederationEnvelope();
  process.stdout.write(`${JSON.stringify(out, null, 2)}\n`);
}
