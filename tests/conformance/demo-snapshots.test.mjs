import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { runCapabilityExpiredChainDemo } from '../../scripts/capability-expired-chain.mjs';
import { runCapabilityScopeFailureDemo } from '../../scripts/capability-scope-failure.mjs';
import { runCapabilityValidChainDemo } from '../../scripts/capability-valid-chain.mjs';
import { runEVRCapabilityTrace } from '../../scripts/evr-capability-trace.mjs';
import { runEVRResolveTrace } from '../../scripts/evr-resolve-trace.mjs';
import { runEVRRouteTrace } from '../../scripts/evr-route-trace.mjs';
import { runFederationConvergenceWitnessDemo } from '../../scripts/federation-convergence-witness.mjs';
import { runFederationDeterministicArbitrationDemo } from '../../scripts/federation-deterministic-arbitration.mjs';
import { runFederationDivergenceWitnessDemo } from '../../scripts/federation-divergence-witness.mjs';
import { runFederationMultiProviderDemo } from '../../scripts/federation-multi-provider.mjs';
import { runHubRoleShellDemo } from '../../scripts/hub-role-shell-demo.mjs';
import { runMinimalFederationProof } from '../../scripts/minimal-federation-proof.mjs';
import { runRevocationAncestorRevokedDemo } from '../../scripts/revocation-ancestor-revoked.mjs';
import { runRevocationUnauthorizedAttemptDemo } from '../../scripts/revocation-unauthorized-attempt.mjs';
import { runRevocationValidThenRevokedDemo } from '../../scripts/revocation-valid-then-revoked.mjs';
import { runHelloTetragrammatron } from '../../scripts/hello-tetragrammatron.mjs';
import { runNodePipelineDemo } from '../../scripts/node-pipeline-demo.mjs';

async function loadSnapshot(name) {
  const p = path.join(process.cwd(), 'fixtures', 'demos', `${name}.snapshot.json`);
  return JSON.parse(await readFile(p, 'utf8'));
}

function stablePipelineView(result) {
  return {
    acceptance: result.acceptance,
    adapter: {
      authoritative: result.adapter.value.value.authoritative,
      credential: result.adapter.value.value.credential,
      label: result.adapter.value.value.adapter_label,
    },
    descriptor: {
      descriptor_digest: result.descriptor.value.descriptor.descriptor_digest,
      normal_form_digest: result.descriptor.value.descriptor.normal_form_digest,
      ok: result.descriptor.value.ok,
      schema_digest: result.descriptor.value.schema_digest,
      sid: result.descriptor.value.sid,
    },
    events: {
      count: result.events.value.length,
      kinds: result.events.value.map((e) => e.kind),
    },
    resolve: {
      ok: result.resolve.value.ok,
      sid: result.resolve.value.identity.sid,
      stage: result.resolve.value.stage,
      value_kind: result.resolve.value.value_kind,
    },
    store: {
      entry_count: result.store.value.summary.log_entry_count,
      nrr_verify_ok: result.store.value.summary.nrr_verify_ok,
      replay_readiness_ok: result.store.value.summary.replay_readiness_ok,
    },
  };
}

test('hello demo snapshot is frozen and reproducible', async () => {
  const expected = await loadSnapshot('hello');
  const actualA = await runHelloTetragrammatron();
  const actualB = await runHelloTetragrammatron();

  assert.equal(canonicalJson(actualA), canonicalJson(expected));
  assert.equal(canonicalJson(actualB), canonicalJson(expected));
});

test('pipeline demo snapshot is frozen and reproducible (stable projection)', async () => {
  const expected = await loadSnapshot('pipeline');
  const actualA = stablePipelineView(await runNodePipelineDemo());
  const actualB = stablePipelineView(await runNodePipelineDemo());

  assert.equal(canonicalJson(actualA), canonicalJson(expected));
  assert.equal(canonicalJson(actualB), canonicalJson(expected));
});

test('federation demo snapshot is frozen and reproducible', async () => {
  const expected = await loadSnapshot('federation');
  const actualA = await runMinimalFederationProof();
  const actualB = await runMinimalFederationProof();

  assert.equal(canonicalJson(actualA), canonicalJson(expected));
  assert.equal(canonicalJson(actualB), canonicalJson(expected));
});

test('capability demos snapshots are frozen and reproducible', async () => {
  const expectedValid = await loadSnapshot('capability-valid');
  const expectedExpired = await loadSnapshot('capability-expired');
  const expectedScope = await loadSnapshot('capability-scope');

  const validA = await runCapabilityValidChainDemo();
  const validB = await runCapabilityValidChainDemo();
  const expiredA = await runCapabilityExpiredChainDemo();
  const expiredB = await runCapabilityExpiredChainDemo();
  const scopeA = await runCapabilityScopeFailureDemo();
  const scopeB = await runCapabilityScopeFailureDemo();

  assert.equal(canonicalJson(validA), canonicalJson(expectedValid));
  assert.equal(canonicalJson(validB), canonicalJson(expectedValid));
  assert.equal(canonicalJson(expiredA), canonicalJson(expectedExpired));
  assert.equal(canonicalJson(expiredB), canonicalJson(expectedExpired));
  assert.equal(canonicalJson(scopeA), canonicalJson(expectedScope));
  assert.equal(canonicalJson(scopeB), canonicalJson(expectedScope));
});

test('EVR demos snapshots are frozen and reproducible', async () => {
  const expectedResolve = await loadSnapshot('evr-resolve-trace');
  const expectedCapability = await loadSnapshot('evr-capability-trace');
  const expectedRoute = await loadSnapshot('evr-route-trace');

  const resolveA = await runEVRResolveTrace();
  const resolveB = await runEVRResolveTrace();
  const capA = await runEVRCapabilityTrace();
  const capB = await runEVRCapabilityTrace();
  const routeA = await runEVRRouteTrace();
  const routeB = await runEVRRouteTrace();

  assert.equal(canonicalJson(resolveA), canonicalJson(expectedResolve));
  assert.equal(canonicalJson(resolveB), canonicalJson(expectedResolve));
  assert.equal(canonicalJson(capA), canonicalJson(expectedCapability));
  assert.equal(canonicalJson(capB), canonicalJson(expectedCapability));
  assert.equal(canonicalJson(routeA), canonicalJson(expectedRoute));
  assert.equal(canonicalJson(routeB), canonicalJson(expectedRoute));
});

test('federation breadth demos snapshots are frozen and reproducible', async () => {
  const expectedMulti = await loadSnapshot('federation-multi-provider');
  const expectedArb = await loadSnapshot('federation-deterministic-arbitration');
  const expectedConv = await loadSnapshot('federation-convergence-witness');
  const expectedDiv = await loadSnapshot('federation-divergence-witness');

  const multiA = await runFederationMultiProviderDemo();
  const multiB = await runFederationMultiProviderDemo();
  const arbA = await runFederationDeterministicArbitrationDemo();
  const arbB = await runFederationDeterministicArbitrationDemo();
  const convA = await runFederationConvergenceWitnessDemo();
  const convB = await runFederationConvergenceWitnessDemo();
  const divA = await runFederationDivergenceWitnessDemo();
  const divB = await runFederationDivergenceWitnessDemo();

  assert.equal(canonicalJson(multiA), canonicalJson(expectedMulti));
  assert.equal(canonicalJson(multiB), canonicalJson(expectedMulti));
  assert.equal(canonicalJson(arbA), canonicalJson(expectedArb));
  assert.equal(canonicalJson(arbB), canonicalJson(expectedArb));
  assert.equal(canonicalJson(convA), canonicalJson(expectedConv));
  assert.equal(canonicalJson(convB), canonicalJson(expectedConv));
  assert.equal(canonicalJson(divA), canonicalJson(expectedDiv));
  assert.equal(canonicalJson(divB), canonicalJson(expectedDiv));
});

test('revocation demos snapshots are frozen and reproducible', async () => {
  const expectedRevoked = await loadSnapshot('revocation-valid-then-revoked');
  const expectedAncestor = await loadSnapshot('revocation-ancestor-revoked');
  const expectedUnauthorized = await loadSnapshot('revocation-unauthorized-attempt');

  const revokedA = await runRevocationValidThenRevokedDemo();
  const revokedB = await runRevocationValidThenRevokedDemo();
  const ancestorA = await runRevocationAncestorRevokedDemo();
  const ancestorB = await runRevocationAncestorRevokedDemo();
  const unauthorizedA = await runRevocationUnauthorizedAttemptDemo();
  const unauthorizedB = await runRevocationUnauthorizedAttemptDemo();

  assert.equal(canonicalJson(revokedA), canonicalJson(expectedRevoked));
  assert.equal(canonicalJson(revokedB), canonicalJson(expectedRevoked));
  assert.equal(canonicalJson(ancestorA), canonicalJson(expectedAncestor));
  assert.equal(canonicalJson(ancestorB), canonicalJson(expectedAncestor));
  assert.equal(canonicalJson(unauthorizedA), canonicalJson(expectedUnauthorized));
  assert.equal(canonicalJson(unauthorizedB), canonicalJson(expectedUnauthorized));
});

test('hub role-shell demo snapshot is frozen and reproducible', async () => {
  const expected = await loadSnapshot('hub-role-shell');

  const actualA = await runHubRoleShellDemo();
  const actualB = await runHubRoleShellDemo();

  assert.equal(canonicalJson(actualA), canonicalJson(expected));
  assert.equal(canonicalJson(actualB), canonicalJson(expected));
});
