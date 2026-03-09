import { test } from 'node:test';
import assert from 'node:assert/strict';

import { runMinimalFederationProof } from '../../scripts/minimal-federation-proof.mjs';

test('minimal federation call equals direct host result', async () => {
  const result = await runMinimalFederationProof();

  assert.equal(result.equivalence, true);
  assert.equal(typeof result.route_target, 'string');
});

test('unknown SID route fails deterministically', async () => {
  const result = await runMinimalFederationProof();
  assert.equal(result.unknown_route_result.ok, false);
  assert.equal(result.unknown_route_result.code, 'route_not_found');
});
