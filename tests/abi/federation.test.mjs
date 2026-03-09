import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeStructure } from '../../src/abi/encode.mjs';
import {
  toArbitrationResultAbi,
  toConvergenceWitnessAbi,
  toDivergenceWitnessAbi,
  toFederationAnnouncementAbi,
  toRouteSetAbi,
} from '../../src/abi/schema.mjs';
import { arbitrateRoute } from '../../src/federation/arbitrate.mjs';
import { deriveRouteSet } from '../../src/federation/routeset.mjs';
import { makeAnnouncement, makeDescriptor } from '../federation/fixture.mjs';

test('federation structures map into ABI', () => {
  const descriptor = makeDescriptor({
    endpoint: 'node-a',
    provider_id: 'provider:a',
    schema_digests: ['sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8'],
  });
  const ann = makeAnnouncement({ descriptor, provider_id: 'provider:a' });

  const annAbi = toFederationAnnouncementAbi(ann);
  assert.equal(encodeStructure('FederationAnnouncement', annAbi).ok, true);

  const routes = deriveRouteSet([ann], {
    federation_scope: '',
    schema_digest: descriptor.schema_digests[0],
    sid: 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    stage: 'Normalized',
  }, { now_epoch: 20 });
  assert.equal(routes.ok, true);

  const routeAbi = toRouteSetAbi(routes.value);
  assert.equal(encodeStructure('RouteSet', routeAbi).ok, true);

  const arbitration = arbitrateRoute(routes.value);
  assert.equal(arbitration.ok, true);
  const arbAbi = toArbitrationResultAbi(arbitration.value);
  assert.equal(encodeStructure('ArbitrationResult', arbAbi).ok, true);

  const convAbi = toConvergenceWitnessAbi({
    call_digest: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    result_digest: 'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
    runtime_sids: [
      'sid:dbc:1111111111111111111111111111111111111111111111111111111111111111',
      'sid:dbc:2222222222222222222222222222222222222222222222222222222222222222',
    ],
    stage: 'Normalized',
    test_id: 'conv',
  });
  assert.equal(encodeStructure('ConvergenceWitness', convAbi).ok, true);

  const divAbi = toDivergenceWitnessAbi({
    call_digest: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    divergence_stage: 'Normalized',
    result_digests: {
      'sid:dbc:1111111111111111111111111111111111111111111111111111111111111111': 'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
    },
    runtime_sids: [
      'sid:dbc:1111111111111111111111111111111111111111111111111111111111111111',
      'sid:dbc:2222222222222222222222222222222222222222222222222222222222222222',
    ],
    test_id: 'div',
  });
  assert.equal(encodeStructure('DivergenceWitness', divAbi).ok, true);
});
