import { test } from 'node:test';
import assert from 'node:assert/strict';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { deriveRouteSet } from '../../src/federation/routeset.mjs';
import { FED, loadFederationCases, makeAnnouncement, makeDescriptor, makeRequest } from './fixture.mjs';

test('same announcement set yields same route set deterministically', async () => {
  const det = await loadFederationCases('determinism');
  assert.equal(det.length >= 4, true);

  const a = makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a' }), provider_id: 'provider:a' });
  const b = makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:b', priority: 1 }), provider_id: 'provider:b' });

  const request = makeRequest();
  const first = deriveRouteSet([a, b], request, { now_epoch: 20 });
  const second = deriveRouteSet([b, a], request, { now_epoch: 20 });

  assert.equal(first.ok, true);
  assert.equal(second.ok, true);
  assert.equal(canonicalJson(first.value.candidates), canonicalJson(second.value.candidates));
});

test('route set rejects conflicts, unreachable and mismatch deterministically', async () => {
  const dA = makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a' });
  const dB = makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:a' });
  const conflict = deriveRouteSet([
    makeAnnouncement({ descriptor: dA, provider_id: 'provider:a' }),
    makeAnnouncement({ descriptor: dB, provider_id: 'provider:a' }),
  ], makeRequest(), { now_epoch: 20 });
  assert.equal(conflict.ok, false);
  assert.equal(conflict.code, 'announcement_conflict');

  const unreachable = deriveRouteSet([
    makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', supported_stages: ['Projected'] }), provider_id: 'provider:a' }),
  ], makeRequest(), { now_epoch: 20 });
  assert.equal(unreachable.ok, false);
  assert.equal(unreachable.code, 'route_unreachable');

  const scopeMismatch = deriveRouteSet([
    makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', federation_scope: 'federation:other', provider_id: 'provider:a' }), provider_id: 'provider:a' }),
  ], makeRequest({ federation_scope: 'federation:current' }), { now_epoch: 20 });
  assert.equal(scopeMismatch.ok, false);
  assert.equal(scopeMismatch.code, 'route_unreachable');

  const schemaMismatch = deriveRouteSet([
    makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', schema_digests: [FED.schemaB] }), provider_id: 'provider:a' }),
  ], makeRequest({ schema_digest: FED.schemaA }), { now_epoch: 20 });
  assert.equal(schemaMismatch.ok, false);
  assert.equal(schemaMismatch.code, 'route_unreachable');
});
