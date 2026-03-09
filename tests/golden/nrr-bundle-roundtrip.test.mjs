import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { NRR } from '../../src/substrate/nrr.mjs';
import { buildRepoFromFixture, loadGoldenFixture, reducer } from '../support/nrr-fixture.mjs';

test('NRR-G-04: export -> import -> replay yields identical final state', async () => {
  const fixture = await loadGoldenFixture();

  const sourcePath = await mkdtemp(path.join(os.tmpdir(), 'nrr-src-'));
  const targetPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-dst-'));

  const { nrr: source, state: sourceState } = await buildRepoFromFixture(sourcePath, fixture);
  const bundle = await source.exportBundle();

  const imported = new NRR(targetPath);
  await imported.init();
  await imported.importBundle(bundle);

  const importedState = await imported.replay(reducer, fixture.initial_state);

  assert.deepEqual(importedState, sourceState);
  assert.deepEqual(await imported.log(), await source.log());
});

test('verify(): returns structured store-integrity report', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-verify-'));

  const { nrr } = await buildRepoFromFixture(repoPath, fixture);
  const report = await nrr.verify();

  assert.equal(typeof report, 'object');
  assert.equal(report.ok, true);
  assert.equal(report.manifest.ok, true);
  assert.equal(report.log.ok, true);
  assert.equal(report.blobs.ok, true);
  assert.equal(report.replay_readiness.ok, true);
});
