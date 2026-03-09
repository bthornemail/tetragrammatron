import { test } from 'node:test';
import assert from 'node:assert/strict';

import { runFederationConvergenceWitnessDemo } from '../../scripts/federation-convergence-witness.mjs';
import { runFederationDeterministicArbitrationDemo } from '../../scripts/federation-deterministic-arbitration.mjs';
import { runFederationDivergenceWitnessDemo } from '../../scripts/federation-divergence-witness.mjs';
import { runFederationMultiProviderDemo } from '../../scripts/federation-multi-provider.mjs';
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

test('multi-provider federation demo proves deterministic routing pipeline', async () => {
  const result = await runFederationMultiProviderDemo();
  assert.equal(result.equivalence, true);
  assert.equal(result.provider_count >= 2, true);
  assert.equal(typeof result.selected_provider, 'string');
});

test('deterministic arbitration demo is stable across announcement orders', async () => {
  const result = await runFederationDeterministicArbitrationDemo();
  assert.equal(result.same_selected_provider, true);
  assert.equal(result.selected_provider, 'provider:b');
});

test('convergence/divergence witness demos emit deterministic witness kinds', async () => {
  const converged = await runFederationConvergenceWitnessDemo();
  const diverged = await runFederationDivergenceWitnessDemo();

  assert.equal(converged.ok, true);
  assert.equal(converged.witness_kind, 'convergence_witness');
  assert.equal(diverged.ok, false);
  assert.equal(diverged.witness_kind, 'divergence_witness');
});
