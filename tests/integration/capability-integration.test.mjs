import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { NRR } from '../../src/substrate/nrr.mjs';
import { demoResolveCall } from '../../scripts/_shared.mjs';
import { SIDS, buildValidSingle, loadCaseIds } from '../capability/fixture.mjs';

function validCapabilityContext() {
  return {
    capability_chain: buildValidSingle(),
    now_epoch: 20,
    trust_anchors: [SIDS.govRoot],
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
  };
}

test('resolve under valid capability succeeds and invalid capability fails deterministically', async () => {
  const cases = await loadCaseIds('integration');
  assert.equal(cases.length >= 5, true);

  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'cap-int-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const denied = await host.resolve({
    ...demoResolveCall(),
    required_capability: true,
    capability_context: {},
  });
  assert.equal(denied.ok, false);
  assert.equal(denied.code, 'capability_denied');

  const allowed = await host.resolve({
    ...demoResolveCall(),
    required_capability: true,
    capability_context: validCapabilityContext(),
  });
  assert.equal(allowed.ok, true);
  assert.equal(allowed.identity?.sid?.startsWith('sid:dbc:'), true);
});

test('capability verification evidence survives bundle export/import replay', async () => {
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'cap-int-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'cap-int-b-'));

  const hostA = await CoreHost.create({ repoDir: repoA });
  const verifiedA = await hostA.verifyCapability(validCapabilityContext());
  assert.equal(verifiedA.ok, true);
  assert.equal(verifiedA.status, 'verified');

  const bundle = await hostA.nrr.exportBundle();
  const nrrB = new NRR(repoB);
  await nrrB.init();
  await nrrB.importBundle(bundle);

  const hostB = new CoreHost({ nrr: nrrB });
  const verifiedB = await hostB.verifyCapability(validCapabilityContext());
  assert.equal(verifiedB.ok, true);
  assert.equal(verifiedB.status, 'verified');
});
