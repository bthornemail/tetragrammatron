import { test } from 'node:test';
import assert from 'node:assert/strict';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { arbitrateRoute } from '../../src/federation/arbitrate.mjs';
import { deriveRouteSet } from '../../src/federation/routeset.mjs';
import { makeAnnouncement, makeDescriptor, makeRequest } from './fixture.mjs';

function routeSetFromAnnouncements(announcements, request = makeRequest()) {
  const derived = deriveRouteSet(announcements, request, { now_epoch: 20 });
  assert.equal(derived.ok, true);
  return derived.value;
}

test('same topology yields same deterministic arbitration winner', async () => {
  const announcements = [
    makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', priority: 1 }), provider_id: 'provider:a' }),
    makeAnnouncement({ descriptor: makeDescriptor({ endpoint: 'node-b', provider_id: 'provider:b', priority: 2 }), provider_id: 'provider:b' }),
  ];

  const set = routeSetFromAnnouncements(announcements);
  const a = arbitrateRoute(set);
  const b = arbitrateRoute(set);

  assert.equal(a.ok, true);
  assert.equal(b.ok, true);
  assert.equal(a.value.selected.provider_id, 'provider:b');
  assert.equal(canonicalJson(a.value), canonicalJson(b.value));
});

test('ambiguous route set yields deterministic typed ambiguity failure', async () => {
  const candidates = [
    { descriptor_digest: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', endpoint: 'node-a', federation_scope: '', priority: 1, provider_id: 'provider:a' },
    { descriptor_digest: 'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', endpoint: 'node-b', federation_scope: '', priority: 1, provider_id: 'provider:b' },
  ];

  const result = arbitrateRoute({ candidates, request: makeRequest() }, { ambiguity_policy: 'fail' });
  assert.equal(result.ok, false);
  assert.equal(result.code, 'route_ambiguity');
  assert.equal(Array.isArray(result.evidence.candidates), true);
});
